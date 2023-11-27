open Quantity
open Yojson.Basic.Util
open Yojson.Basic
open Jsonutil

type t = { name : string; default_units : Quantity.units }

(* Convert JSON to ingredient type *)
let of_json json =
  {
    name = json |> member "name" |> string_of_mem;
    default_units =
      (let units_string = json |> member "default_units" |> string_of_mem in
       match Quantity.units_of_string units_string with
       | Some u -> u
       | None -> failwith ("Could not find units " ^ units_string));
  }

(* Function to parse a list of ingredients from a JSON string *)
let ingredients_from_file file =
  let json = from_file file in
  (* Match the JSON structure to a list, then convert each element *)
  match json with
  | `List lst -> List.map of_json lst
  | _ -> failwith "Expected a JSON list"

let all_ingredients =
  let file_names = "data/ingredients" |> Sys.readdir |> Array.to_list in
  List.fold_left
    (fun acc elt -> ingredients_from_file ("data/ingredients/" ^ elt) @ acc)
    [] file_names

let of_string s =
  let s = String.lowercase_ascii s in
  List.find_opt
    (fun ingredient -> String.lowercase_ascii ingredient.name = s)
    all_ingredients

let to_string ingredient = ingredient.name
let compare_names a b = compare (to_string a) (to_string b)
let default_units ing = ing.default_units
