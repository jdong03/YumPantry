open Quantity

type ingredient =
  | Apple
  | Beef
  | Cucumber
  | Milk
  | Corn
  | Basil
  | OliveOil
  | Salt
  | Invalid

type measurement_type = Mass | Volume | Count

let of_string s =
  match String.lowercase_ascii s with
  | "apple" -> Some Apple
  | "beef" -> Some Beef
  | "cucumber" -> Some Cucumber
  | "milk" -> Some Milk
  | "corn" -> Some Corn
  | "basil" -> Some Basil
  | "olive oil" -> Some OliveOil
  | "salt" -> Some Salt
  | _ -> None

let to_string = function
  | Apple -> "apple"
  | Beef -> "beef"
  | Milk -> "milk"
  | Cucumber -> "cucumber"
  | Corn -> "corn"
  | Basil -> "basil"
  | OliveOil -> "olive oil"
  | Salt -> "salt"
  | Invalid -> ""

let compare_names a b = compare (to_string a) (to_string b)

let correct_measurement_type ing =
  match ing with
  | Apple -> Count
  | Beef -> Mass
  | Milk -> Volume
  | Cucumber -> Count
  | Corn -> Count
  | Basil -> Volume
  | OliveOil -> Volume
  | Salt -> Volume
  | Invalid -> Count
