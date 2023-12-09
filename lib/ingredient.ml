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

let to_string ingredient = ingredient.name

(* [calc_score s1 s2] calculates the Levenshtein distance of s1 and s2. *)
let calc_score s1 s2 =
  let len_s1 = String.length s1 in
  let len_s2 = String.length s2 in
  let matrix = Array.make_matrix (len_s1 + 1) (len_s2 + 1) 0 in

  for i = 0 to len_s1 do
    matrix.(i).(0) <- i
  done;

  for j = 0 to len_s2 do
    matrix.(0).(j) <- j
  done;

  for i = 1 to len_s1 do
    for j = 1 to len_s2 do
      let cost = if s1.[i - 1] = s2.[j - 1] then 0 else 1 in
      matrix.(i).(j) <-
        min
          (min (matrix.(i - 1).(j) + 1) (matrix.(i).(j - 1) + 1))
          (matrix.(i - 1).(j - 1) + cost)
    done
  done;

  matrix.(len_s1).(len_s2)

(* [match_string s] "autocorrects" s by matching it to an ingredient in
   Ingredient.al_ingredients. Returns the ingredient that has the lowest
   Levenshtein distance with s if the distance is less than 3.
    Returns None otherwise. *)
let match_string (s : string) : t option =
  let rec match_string_helper s lst acc =
    match lst with
    | [] -> acc
    | h :: t -> (
        let score = calc_score s (String.lowercase_ascii (to_string h)) in
        match acc with
        | num, ing ->
            if score < num then match_string_helper s t (score, h)
            else match_string_helper s t acc)
  in
  let first_ingredient = List.hd all_ingredients in
  match
    match_string_helper s all_ingredients (String.length s, first_ingredient)
  with
  | score, ing -> if score < 3 then Some ing else None

let of_string s =
  let s = String.lowercase_ascii s in
  match_string s

let compare_names a b = compare (to_string a) (to_string b)
let default_units ing = ing.default_units
