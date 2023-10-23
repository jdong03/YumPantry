module type Measurement = sig
  type units
  type measure

  val simplify : measure -> measure
  val convert : measure -> units -> measure
  val add : measure -> measure -> measure
  val subtract : measure -> measure -> measure
  val scale : float -> measure -> measure
  val greater_than : measure -> measure -> bool
  val less_than : measure -> measure -> bool
  val equivalent : measure -> measure -> bool
  val of_string : string -> measure option
  val to_string : measure -> string
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

  val unit_of_string : string -> units option
  val unit_to_string : units -> string
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
    let measure_in_finest_units = UnitMap.find units conversion_map in
    let filtered_map =
      let is_mult x y =
        match modf (x /. y) with remainder, _ -> remainder = 0.0
      in
      UnitMap.filter
        (fun _ value -> is_mult value measure_in_finest_units)
        conversion_map
    in
    match UnitMap.max_binding filtered_map with
    | largest_unit, _ -> convert (m, units) largest_unit

  let add (m1, units1) (m2, units2) =
    assert (UnitMap.mem units1 conversion_map);
    assert (UnitMap.mem units2 conversion_map);
    let a = UnitMap.find units1 conversion_map in
    let b = UnitMap.find units2 conversion_map in
    if a < b then
      (* units1 is 'finer' *)
      let m_b_converted = m2 *. (b /. a) in
      (m1 +. m_b_converted, units1)
    else
      (* units2 is 'finer *)
      let m_a_converted = m1 *. (a /. b) in
      (m_a_converted +. m2, units2)

  (* FIXME: I believe there is an issue here. Try 1 Quart - 1 Gallon *)
  let subtract (m1, units1) (m2, units2) =
    assert (UnitMap.mem units1 conversion_map);
    assert (UnitMap.mem units2 conversion_map);
    let a = UnitMap.find units1 conversion_map in
    let b = UnitMap.find units2 conversion_map in
    if a < b then
      (* units1 is 'finer' *)
      let m_b_converted = m2 *. (b /. a) in
      (m1 -. m_b_converted, units1)
    else
      (* units2 is 'finer *)
      let m_a_converted = m1 *. (a /. b) in
      (m_a_converted -. m2, units2)

  let scale f (m, units) = (f *. m, units)

  let greater_than (m1, units1) (m2, units2) =
    assert (UnitMap.mem units1 conversion_map);
    assert (UnitMap.mem units2 conversion_map);
    (* Convert both measurements to equivalent measurements in the finest unit
       and see if m1 is < m2 *)
    let finest_unit, _ = UnitMap.min_binding conversion_map in
    let m1_converted, _ = convert (m1, units1) finest_unit in
    let m2_converted, _ = convert (m2, units2) finest_unit in
    if m1_converted > m2_converted then true else false

  let less_than (m1, units1) (m2, units2) =
    assert (UnitMap.mem units1 conversion_map);
    assert (UnitMap.mem units2 conversion_map);
    (* Convert both measurements to equivalent measurements in the finest unit
       and see if m1 is < m2 *)
    let finest_unit, _ = UnitMap.min_binding conversion_map in
    let m1_converted, _ = convert (m1, units1) finest_unit in
    let m2_converted, _ = convert (m2, units2) finest_unit in
    if m1_converted < m2_converted then true else false

  let equivalent (m1, units1) (m2, units2) =
    assert (UnitMap.mem units1 conversion_map);
    assert (UnitMap.mem units2 conversion_map);
    (* Convert both measurements to equivalent measurements in the finest unit
       and see if they are the same *)
    let finest_unit, _ = UnitMap.min_binding conversion_map in
    let m1_converted, _ = convert (m1, units1) finest_unit in
    let m2_converted, _ = convert (m2, units2) finest_unit in
    if m1_converted = m2_converted then true else false

  (** FIXME: this is yucky *)
  let of_string s =
    match String.split_on_char ' ' s with
    | [ m; units ] -> (
        match Float.of_string_opt m with
        | Some m -> (
            match M.unit_of_string units with
            | Some units -> Some (m, units)
            | None -> None)
        | None -> None)
    | _ -> None

  let to_string (q, units) = Float.to_string q ^ M.unit_to_string units
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
    |> UnitMap.add QuarterCup 12.0
    |> UnitMap.add HalfCup 24.0
    |> UnitMap.add Cup 48.0
    |> UnitMap.add Pint 96.0
    |> UnitMap.add Quart 192.0
    |> UnitMap.add Gallon 768.0
    ) [@ocamlformat "disable"]

  let unit_of_string s =
    match String.lowercase_ascii s with
    | "teaspoon" -> Some Teaspoon
    | "tablespoon" -> Some Tablespoon
    | "quarter cup" -> Some QuarterCup
    | "half cup" -> Some HalfCup
    | "cup" -> Some Cup
    | "pint" -> Some Pint
    | "quart" -> Some Quart
    | "gallon" -> Some Gallon
    | _ -> None

  let unit_to_string u =
    match u with
    | Teaspoon -> "teaspoon"
    | Tablespoon -> "tablespoon"
    | QuarterCup -> "quart cup"
    | HalfCup -> "half cup"
    | Cup -> "cup"
    | Pint -> "pint"
    | Quart -> "quart"
    | Gallon -> "gallon"
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

  let unit_of_string s =
    match String.lowercase_ascii s with
    | "ounce" -> Some Ounce
    | "pound" -> Some Pound
    | _ -> None

  let unit_to_string u = match u with Ounce -> "ounce" | Pound -> "pound"
end)

(* Amount *)

type amount = Volume of Volume.measure | Mass of Mass.measure | Count of float

(** TODO: this is ugly *)
let of_string s : amount option =
  match String.split_on_char ' ' s with
  | [ m; units ] -> (
      if Volume.of_string s = None then
        (* Try making a Mass *)
        match Mass.of_string s with
        | Some (m, units) -> Some (Mass (m, units))
        | None -> None
      else
        (* Make a Volume *)
        match Volume.of_string s with
        | Some (v, units) -> Some (Volume (v, units))
        | None -> None)
  | [ m ] -> (
      (* Try making a Count *)
      match Float.of_string_opt m with Some m -> Some (Count m) | _ -> None)
  | _ -> None
