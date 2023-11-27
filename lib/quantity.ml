module type Quantity = sig
  type t
  type units

  val simplify : t -> t
  val add : t -> t -> t option
  val subtract : t -> t -> t option
  val scale : float -> t -> t
  val greater_than : t -> t -> bool option
  val less_than : t -> t -> bool option
  val equivalent : t -> t -> bool option
  val of_string : string -> t option
  val to_string : t -> string
  val same_type_of_units : units -> units -> bool
  val units_of_string : string -> units option
  val units_of_quantity : t -> units
  val measurement_type : units -> string
  val is_neg : t -> bool
end

(** Restart ******************************************************************)

type volume_units =
  | Teaspoon
  | Tablespoon
  | QuarterCup
  | HalfCup
  | Cup
  | Pint
  | Quart
  | Gallon

type mass_units = Ounce | Pound
type count_units = Slice | Bulb | Whole

let convert_volume amount from_unit to_unit =
  let volume_unit_to_teaspoons = function
    | Teaspoon -> 1.0
    | Tablespoon -> 3.0
    | QuarterCup -> 12.0
    | HalfCup -> 24.0
    | Cup -> 48.0
    | Pint -> 96.0
    | Quart -> 192.0
    | Gallon -> 768.0
  in
  let amount_in_teaspoons = amount *. volume_unit_to_teaspoons from_unit in
  amount_in_teaspoons /. volume_unit_to_teaspoons to_unit

let convert_mass amount from_unit to_unit =
  let mass_unit_to_ounces = function Ounce -> 1.0 | Pound -> 16.0 in
  let amount_in_ounces = amount *. mass_unit_to_ounces from_unit in
  amount_in_ounces /. mass_unit_to_ounces to_unit

(** Groups of units (cannot be mixed)*)
type units =
  | VolumeUnit of volume_units
  | MassUnit of mass_units
  | CountUnit of count_units

type t = float * units

(** TODO: this is so messy! fix this! *)
let simplify (q : t) : t =
  match q with
  | v, VolumeUnit u ->
      let teaspoons = convert_volume v u Teaspoon in
      if teaspoons >= 768.0 then (teaspoons /. 768.0, VolumeUnit Gallon)
      else if teaspoons >= 192.0 then (teaspoons /. 192.0, VolumeUnit Quart)
      else if teaspoons >= 96.0 then (teaspoons /. 96.0, VolumeUnit Pint)
      else if teaspoons >= 48.0 then (teaspoons /. 48.0, VolumeUnit Cup)
      else if teaspoons >= 24.0 then (teaspoons /. 24.0, VolumeUnit HalfCup)
      else if teaspoons >= 12.0 then (teaspoons /. 12.0, VolumeUnit QuarterCup)
      else if teaspoons >= 3.0 then (teaspoons /. 3.0, VolumeUnit Tablespoon)
      else (v, VolumeUnit Teaspoon)
  | m, MassUnit u -> (m, MassUnit u)
  | c, CountUnit u -> (c, CountUnit u)

let add q1 q2 =
  match (q1, q2) with
  | (v1, VolumeUnit u1), (v2, VolumeUnit u2) ->
      let v2_converted = convert_volume v2 u2 u1 in
      Some (v1 +. v2_converted, VolumeUnit u1)
  | (m1, MassUnit u1), (m2, MassUnit u2) ->
      let m2_converted = convert_mass m2 u2 u1 in
      Some (m1 +. m2_converted, MassUnit u1)
  | (c1, CountUnit u1), (c2, CountUnit u2) ->
      if u1 <> u2 then None else Some (c1 +. c2, CountUnit u1)
  | _ -> None

let subtract q1 q2 =
  match (q1, q2) with
  | (v1, VolumeUnit u1), (v2, VolumeUnit u2) ->
      let v2_converted = convert_volume v2 u2 u1 in
      Some (v1 -. v2_converted, VolumeUnit u1)
  | (m1, MassUnit u1), (m2, MassUnit u2) ->
      let m2_converted = convert_mass m2 u2 u1 in
      Some (m1 -. m2_converted, MassUnit u1)
  | (c1, CountUnit u1), (c2, CountUnit u2) ->
      if u1 <> u2 then None else Some (c1 -. c2, CountUnit u1)
  | _ -> None

let scale f q =
  match q with
  | v, VolumeUnit u -> (v *. f, VolumeUnit u)
  | m, MassUnit u -> (m *. f, MassUnit u)
  | c, CountUnit u -> (c *. f, CountUnit u)

let greater_than q1 q2 =
  match (q1, q2) with
  | (v1, VolumeUnit u1), (v2, VolumeUnit u2) ->
      let v2_converted = convert_volume v2 u2 u1 in
      Some (v1 > v2_converted)
  | (m1, MassUnit u1), (m2, MassUnit u2) ->
      let m2_converted = convert_mass m2 u2 u1 in
      Some (m1 > m2_converted)
  | (c1, CountUnit u1), (c2, CountUnit u2) ->
      if u1 <> u2 then None else Some (c1 > c2)
  | _ -> None

let less_than q1 q2 =
  match (q1, q2) with
  | (v1, VolumeUnit u1), (v2, VolumeUnit u2) ->
      let v2_converted = convert_volume v2 u2 u1 in
      Some (v1 < v2_converted)
  | (m1, MassUnit u1), (m2, MassUnit u2) ->
      let m2_converted = convert_mass m2 u2 u1 in
      Some (m1 < m2_converted)
  | (c1, CountUnit u1), (c2, CountUnit u2) ->
      if u1 <> u2 then None else Some (c1 < c2)
  | _ -> None

let equivalent q1 q2 =
  match (q1, q2) with
  | (v1, VolumeUnit u1), (v2, VolumeUnit u2) ->
      let v2_converted = convert_volume v2 u2 u1 in
      Some (v1 = v2_converted)
  | (m1, MassUnit u1), (m2, MassUnit u2) ->
      let m2_converted = convert_mass m2 u2 u1 in
      Some (m1 = m2_converted)
  | (c1, CountUnit u1), (c2, CountUnit u2) ->
      if u1 <> u2 then None else Some (c1 = c2)
  | _ -> None

let sanitize_string s =
  let remove_spaces s = String.concat "" (String.split_on_char ' ' s) in
  let remove_last_if_s s =
    let len = String.length s in
    if len > 0 && s.[len - 1] = 's' then String.sub s 0 (len - 1) else s
  in
  String.trim s |> String.lowercase_ascii |> remove_spaces |> remove_last_if_s

let volume_unit_of_string s : volume_units option =
  match sanitize_string s with
  | "teaspoon" -> Some Teaspoon
  | "tablespoon" -> Some Tablespoon
  | "quartercup" -> Some QuarterCup
  | "halfcup" -> Some HalfCup
  | "cup" -> Some Cup
  | "pint" -> Some Pint
  | "quart" -> Some Quart
  | "gallon" -> Some Gallon
  | _ -> None

let mass_unit_of_string s : mass_units option =
  match sanitize_string s with
  | "ounce" -> Some Ounce
  | "pound" -> Some Pound
  | _ -> None

let count_unit_of_string s : count_units option =
  match sanitize_string s with
  | "slice" -> Some Slice
  | "bulb" -> Some Bulb
  | "whole" -> Some Whole
  | _ -> None

let units_of_string s : units option =
  match volume_unit_of_string s with
  | Some v -> Some (VolumeUnit v)
  | None -> (
      match mass_unit_of_string s with
      | Some m -> Some (MassUnit m)
      | None -> (
          match count_unit_of_string s with
          | Some c -> Some (CountUnit c)
          | None -> None))

(** TODO: this is ugly *)
let of_string s : t option =
  match String.split_on_char ' ' s with
  | c :: [] -> (
      (* Assuming single number strings are always counts with unit 'Whole' *)
      match Float.of_string_opt c with
      | Some value -> Some (value, CountUnit Whole)
      | None -> None)
  | m :: units_list -> (
      let units_str = String.concat "" units_list in
      match (Float.of_string_opt m, units_of_string units_str) with
      | Some value, Some unit -> Some (value, unit)
      | _ -> None)
  | _ -> None

let string_of_volume_unit u =
  match u with
  | Teaspoon -> "Teaspoon"
  | Tablespoon -> "Tablespoon"
  | QuarterCup -> "Quarter Cup"
  | HalfCup -> "Half Cup"
  | Cup -> "Cup"
  | Pint -> "Pint"
  | Quart -> "Quart"
  | Gallon -> "Gallon"

let string_of_mass_unit u = match u with Ounce -> "Ounce" | Pound -> "Pound"

let string_of_count_unit u =
  match u with Slice -> "Slice" | Bulb -> "Bulb" | Whole -> "Whole"

let to_string q =
  match q with
  | f, units -> (
      let amount = Float.to_string f in
      let plural_addition = if f = 1.0 then "" else "s" in
      match units with
      | MassUnit u -> amount ^ " " ^ string_of_mass_unit u ^ plural_addition
      | VolumeUnit u -> amount ^ " " ^ string_of_volume_unit u ^ plural_addition
      | CountUnit u ->
          if u = Whole then amount
          else amount ^ " " ^ string_of_count_unit u ^ plural_addition)

let measurement_type u =
  match u with
  | VolumeUnit u -> "Volume"
  | MassUnit u -> "Mass"
  | CountUnit u -> "Count"

let same_type_of_units u1 u2 =
  match (u1, u2) with
  | VolumeUnit u1, VolumeUnit u2 -> true
  | MassUnit u1, MassUnit u2 -> true
  | CountUnit u1, CountUnit u2 -> true
  | _ -> false

let units_of_quantity (f, u) = u
let is_neg (q : t) : bool = match q with a, _ -> a <= 0.0
