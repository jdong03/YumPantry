open Quantity
open Yojson.Basic.Util
open Yojson.Basic

type measurement_type = Mass | Volume | Count
type ingredient = { name : string; measurement_type : measurement_type }

let remove_double_quotes s = String.concat "" (String.split_on_char '\"' s)

let measurement_type_of_string s =
  match s |> String.lowercase_ascii with
  | "mass" -> Mass
  | "volume" -> Volume
  | "count" -> Count
  | _ -> failwith ("Could not find measurement type \"" ^ s ^ "\"")

(* Convert JSON to ingredient type *)
let ingredient_of_json json =
  {
    name = json |> member "name" |> to_string |> remove_double_quotes;
    measurement_type =
      json |> member "measurement_type" |> to_string |> remove_double_quotes
      |> measurement_type_of_string;
  }

(* Function to parse a list of ingredients from a JSON string *)
let ingredients_from_file file =
  let json = from_file file in
  (* Match the JSON structure to a list, then convert each element *)
  match json with
  | `List lst -> List.map ingredient_of_json lst
  | _ -> failwith "Expected a JSON list"

let all_ingredients =
  let file_names = "data" |> Sys.readdir |> Array.to_list in
  List.fold_left
    (fun acc elt -> ingredients_from_file ("data/" ^ elt) @ acc)
    [] file_names

let of_string s =
  let s = String.lowercase_ascii s in
  List.find_opt
    (fun ingredient -> String.lowercase_ascii ingredient.name = s)
    all_ingredients

let to_string ingredient = ingredient.name
let compare_names a b = compare (to_string a) (to_string b)

let string_of_measurement_type = function
  | Mass -> "Mass"
  | Volume -> "Volume"
  | Count -> "Count"
