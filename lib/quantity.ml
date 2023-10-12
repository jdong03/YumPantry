module type Measurement = sig
  type units
  type measure

  val simplify : measure -> measure
  val convert : measure -> units -> measure
  val ( + ) : measure -> measure -> measure
  val ( - ) : measure -> measure -> measure
  val greater_than : measure -> measure -> bool
  val less_than : measure -> measure -> bool
  val equivalent : measure -> measure -> bool
end

(** Define a set of related units *)
module type RelatedUnits = sig
  type units
  (** Possible units *)

  val sizes : units list
  (** Ordered list of units in increasing size*)

  module UnitMap : Map.S with type key = units
  (** Map with units as keys *)

  val conversion_map : float UnitMap.t
  (** Map converting a unit to an equivalent number in the finest unit *)
end

(** A "simple-ish" implementation of [Measurement] using a conversion map*)
module MakeSimpleMeasurement (M : RelatedUnits) :
  Measurement with type units = M.units and type measure = float * M.units =
struct
  type units = M.units
  type measure = float * M.units

  let conversion_map = M.conversion_map

  module UnitMap = M.UnitMap

  let convert (m, units) new_units =
    assert (UnitMap.mem units conversion_map);
    assert (UnitMap.mem new_units conversion_map);
    let measure_in_finest_units = m *. UnitMap.find units conversion_map in
    let new_measure =
      measure_in_finest_units /. UnitMap.find new_units conversion_map
    in
    (new_measure, new_units)

  let simplify (m, units) =
    assert (UnitMap.mem units conversion_map);
    let measure_in_finest_units = m *. UnitMap.find units conversion_map in
    let filtered_map =
      let is_mult x y =
        match modf (x /. y) with remainder, _ -> remainder == 0.0
      in
      UnitMap.filter
        (fun _ value -> is_mult value measure_in_finest_units)
        conversion_map
    in
    (* FIXME: filtered_map is empty sometimes! *)
    match UnitMap.max_binding filtered_map with
    | largest_unit, _ -> convert (m, units) largest_unit

  let ( + ) (m1, units1) (m2, units2) =
    let a = UnitMap.find units1 conversion_map in
    let b = UnitMap.find units2 conversion_map in
    if a < b then
      (* units1 is 'finer' *)
      let m_b_converted = m2 *. (b /. a) in
      simplify (m1 +. m_b_converted, units1)
    else
      (* units2 is 'finer *)
      let m_a_converted = m1 *. (a /. b) in
      simplify (m_a_converted +. m2, units2)

  let ( - ) (m1, units1) (m2, units2) =
    let a = UnitMap.find units1 conversion_map in
    let b = UnitMap.find units2 conversion_map in
    if a < b then
      (* units1 is 'finer' *)
      let m_b_converted = m2 *. (b /. a) in
      simplify (m1 -. m_b_converted, units1)
    else
      (* units2 is 'finer *)
      let m_a_converted = m1 *. (a /. b) in
      simplify (m_a_converted -. m2, units2)

  let greater_than (m1, units1) (m2, units2) =
    match (m1, units1) - (m2, units2) with
    | new_m, _ -> if new_m > 0.0 then true else false

  let less_than (m1, units1) (m1, units2) =
    match (m1, units2) - (m1, units2) with
    | new_m, _ -> if new_m < 0.0 then true else false

  let equivalent (m1, units1) (m2, units2) =
    (* Convert both measurements to equivalent measurements in the finest unit
       and see if they are the same *)
    let finest_unit, _ = UnitMap.min_binding conversion_map in
    let m1_converted, _ = convert (m1, units1) finest_unit in
    let m2_converted, _ = convert (m2, units2) finest_unit in
    if m1_converted = m2_converted then true else false
end

let rec compare_units units_list a b =
  if a = b then 0
  else
    match units_list with
    | [] -> failwith "unit not in size list!"
    | h :: t -> (
        match h with
        | x when x = a -> -1 (*a is smaller than b*)
        | x when x = b -> 1 (*a is larger than b*)
        | _ -> compare_units t a b)

(* Volume *)

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

  (** Map with units as keys *)
  module UnitMap = Map.Make (struct
    type t = units

    (* Some sneaky higher-order stuff *)
    let compare = compare_units sizes
  end)

  (** Map converting a unit to an equivalent number of teaspoons *)
  let conversion_map =
    (
    UnitMap.empty
    |> UnitMap.add Teaspoon 1.0
    |> UnitMap.add Tablespoon 3.0
    |> UnitMap.add QuarterCup 4.0
    |> UnitMap.add HalfCup 8.0
    |> UnitMap.add Cup 16.0
    |> UnitMap.add Pint 32.0
    |> UnitMap.add Quart 64.0
    |> UnitMap.add Gallon 256.0
    ) [@ocamlformat "disable"]
end)

(* Mass *)

type mass_units = Ounce | Pound

module Mass = MakeSimpleMeasurement (struct
  type units = mass_units

  let sizes = [ Ounce; Pound ]

  (** Map with units as keys *)
  module UnitMap = Map.Make (struct
    type t = units

    let compare = compare_units sizes
  end)

  (** Map converting a unit to an equivalent number of ounces  *)
  let conversion_map =
    (
    UnitMap.empty
    |> UnitMap.add Ounce 1.0
    |> UnitMap.add Pound 16.0
    ) [@ocamlformat "disable"]
end)

(* Amount *)

type amount = Volume of Volume.measure | Mass of Mass.measure | Count of float
