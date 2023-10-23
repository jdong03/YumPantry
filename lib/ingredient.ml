open Quantity

type name = Apple | Beef | Cucumber

type measurement_type =
  | Mass of Quantity.Mass.measure
  | Volume of Quantity.Volume.measure
  | Count of float

let of_string s =
  match String.lowercase_ascii s with
  | "apple" -> Some Apple
  | "beef" -> Some Beef
  | "cucumber" -> Some Cucumber
  | _ -> None

let to_string = function
  | Apple -> "apple"
  | Beef -> "beef"
  | Cucumber -> "cucumber"

module MeasurementMap = Map.Make (struct
  type t = name

  (* Some sneaky higher-order stuff *)
  let compare = compare names
end)

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
