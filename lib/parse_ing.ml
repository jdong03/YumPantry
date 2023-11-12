open Yojson.Basic.Util
open Yojson.Basic

type ingredient = { name : string; measurement_type : string }

(* Convert JSON to ingredient type *)
let ingredient_of_json json =
  {
    name = json |> member "name" |> to_string;
    measurement_type = json |> member "measurement_type" |> to_string;
  }

(* Function to parse a list of ingredients from a JSON string *)
let parse_ingredients file =
  (* Read JSON from file *)
  let json = from_file file in
  (* Match the JSON structure to a list, then convert each element *)
  match json with
  | `List lst -> List.map ingredient_of_json lst
  | _ -> failwith "Expected a JSON list"
