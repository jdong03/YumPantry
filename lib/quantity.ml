module type Measurement = sig
  type units
  type measure

  val simplify : measure -> measure
  val convert : measure -> units -> measure
  val ( + ) : measure -> measure -> measure
  val ( - ) : measure -> measure -> measure
  val ( > ) : measure -> measure -> bool
  val ( < ) : measure -> measure -> bool
  val ( == ) : measure -> measure -> bool
end

(** Signature for Measurement implementations using a conversion map *)
module type SimpleMeasurement = sig
  include Measurement
  module UnitMap : Map.S with type key = units

  val conversion_map : float UnitMap.t
end

module type MeasurementType = sig
  type units
  (** Possible units *)

  val sizes : units list
  (** Ordered list of units in increasing size*)

  module UnitMap : Map.S with type key = units
  (** Map with units as keys *)

  val conversion_map : float UnitMap.t
  (** Map converting a unit to an equivalent number in the finest unit *)
end

module MakeSimpleMeasurement (M : MeasurementType) : Measurement = struct
  type units = M.units
  type measure = float * M.units

  let conversion_map = M.conversion_map

  module UnitMap = M.UnitMap

  let convert (m, units) new_units =
    let volume_in_teaspons = m *. UnitMap.find units conversion_map in
    let new_volume =
      volume_in_teaspons /. UnitMap.find new_units conversion_map
    in
    (new_volume, new_units)

  let simplify (m, units) =
    let vol_in_teaspoons = m *. UnitMap.find units conversion_map in
    let filtered_map =
      let is_mult x y =
        match modf (x /. y) with remainder, _ -> remainder == 0.0
      in
      UnitMap.filter
        (fun _ value -> is_mult value vol_in_teaspoons)
        conversion_map
    in
    match UnitMap.max_binding filtered_map with
    | largest_unit, _ -> convert (m, units) largest_unit

  let ( + ) (m1, units1) (m2, units2) =
    let a = UnitMap.find units1 conversion_map in
    let b = UnitMap.find units2 conversion_map in
    if a < b then
      (* vol_unit_a is 'finer' *)
      let vol_b_converted = m2 *. (b /. a) in
      simplify (m1 +. vol_b_converted, units1)
    else
      (* vol_unit_b is 'finer *)
      let vol_a_converted = m1 *. (a /. b) in
      simplify (vol_a_converted +. m2, units2)

  let ( - ) (m1, units1) (m2, units2) =
    let a = UnitMap.find units1 conversion_map in
    let b = UnitMap.find units2 conversion_map in
    if a < b then
      (* vol_unit_a is 'finer' *)
      let vol_b_converted = m2 *. (b /. a) in
      simplify (m1 -. vol_b_converted, units1)
    else
      (* vol_unit_b is 'finer *)
      let vol_a_converted = m1 *. (a /. b) in
      simplify (vol_a_converted -. m2, units2)

  let ( > ) (m1, units1) (m2, units2) =
    match (m1, units1) - (m2, units2) with
    | new_m, _ -> if new_m > 0.0 then true else false

  let ( < ) (m1, units1) (m1, units2) =
    match (m1, units2) - (m1, units2) with
    | new_m, _ -> if new_m < 0.0 then true else false

  let ( == ) (m1, units1) (m2, units2) =
    match (m1, units1) - (m2, units2) with
    | new_m, _ -> if new_m == 0.0 then true else false
end

let rec compare_units units_list a b =
  match units_list with
  | [] -> 0
  | h :: t -> (
      match h with
      | x when x == a -> -1 (*a is smaller than b*)
      | x when x == b -> 1 (*a is larger than b*)
      | _ -> compare_units t a b)

type volume_units =
  | Teaspoon
  | Tablespoon
  | QuarterCup
  | HalfCup
  | Cup
  | Pint
  | Quart
  | Gallon

module Volume = MakeSimpleMeasurement (struct
  type units = volume_units

  let sizes =
    [ Teaspoon; Tablespoon; QuarterCup; HalfCup; Cup; Pint; Quart; Gallon ]

  (** Map with volume_unit as keys *)
  module UnitMap = Map.Make (struct
    type t = units

    (** Some sneaky stuff *)
    let compare = compare_units sizes
  end)

  (** Map converting a unit to an equivalent number of teaspoons *)
  let conversion_map =
    UnitMap.empty |> UnitMap.add Teaspoon 1.0 |> UnitMap.add Tablespoon 3.0
    |> UnitMap.add QuarterCup 4.0 |> UnitMap.add HalfCup 8.0
    |> UnitMap.add Cup 16.0 |> UnitMap.add Pint 32.0 |> UnitMap.add Quart 64.0
    |> UnitMap.add Gallon 256.0
end)

type mass_units = Ounce | Pound

module Mass :
  Measurement with type units = mass_units and type measure = float * mass_units =
MakeSimpleMeasurement (struct
  type units = mass_units

  let sizes = [ Ounce; Pound ]

  (** Map with volume_unit as keys *)
  module UnitMap = Map.Make (struct
    type t = units

    (** Some sneaky stuff *)
    let compare = compare_units sizes
  end)

  (** Map converting a unit to an equivalent number of ounces  *)
  let conversion_map =
    UnitMap.empty |> UnitMap.add Ounce 1.0 |> UnitMap.add Pound 16.0
end)

type amount =
  | Volume of Volume.measure
  | Weight of Mass.measure
  | Count of float
