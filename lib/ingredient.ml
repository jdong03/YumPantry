open Quantity

type ingredient = Apple | Beef | Cucumber | Milk
type measurement_type = Mass | Volume | Count

let of_string s =
  match String.lowercase_ascii s with
  | "apple" -> Some Apple
  | "beef" -> Some Beef
  | "cucumber" -> Some Cucumber
  | "milk" -> Some Milk
  | _ -> None

let to_string = function
  | Apple -> "apple"
  | Beef -> "beef"
  | Milk -> "milk"
  | Cucumber -> "cucumber"

let compare_names a b = compare (to_string a) (to_string b)

module MeasurementMap = Map.Make (struct
  type t = ingredient

  (* Some sneaky higher-order stuff *)
  let compare = compare_names
end)

let measurement_types =
  (
    MeasurementMap.empty
    |> MeasurementMap.add Apple Count
    |> MeasurementMap.add Beef Mass
    |> MeasurementMap.add Milk Volume
    |> MeasurementMap.add Cucumber Count
    ) [@ocamlformat "disable"]

let correct_measurement_type ing =
  match ing with
  | Apple -> Count
  | Beef -> Mass
  | Milk -> Volume
  | Cucumber -> Count
