open Yojson.Basic.Util
open Yojson.Basic
open Jsonutil

type t = {
  title : string;
  servings : int;
  prep_time : string;
  cook_time : string;
  total_time : string;
  ingredients : (Ingredient.t * Quantity.amount) list;
  instructions : string;
}
(** Recipe type *)

(** Parse an ingredient amount pair from JSON *)
let ing_amount_of_json json : Ingredient.t * Quantity.amount =
  let ing = Ingredient.of_json json in
  let quantity =
    json |> member "quantity" |> string_of_mem |> Quantity.of_string
  in
  match quantity with
  | None ->
      failwith
        ("Failed to parse quantity for ingredient " ^ Ingredient.to_string ing)
  | Some quantity -> (ing, quantity)

let recipe_of_json json : t =
  {
    title = json |> member "title" |> string_of_mem;
    servings = json |> member "servings" |> to_int;
    prep_time = json |> member "prep_time" |> string_of_mem;
    cook_time = json |> member "cook_time" |> string_of_mem;
    total_time = json |> member "total_time" |> string_of_mem;
    instructions = json |> member "instructions" |> string_of_mem;
    ingredients =
      json |> member "ingredients" |> to_list |> List.map ing_amount_of_json;
  }

let recipes_from_file file =
  let json = Yojson.Basic.from_file file in
  match json with
  | `List lst -> List.map recipe_of_json lst
  | _ -> failwith "Expected a JSON list"

let all_recipes = recipes_from_file "data/recipes/recipes.json"

let to_string (recipe : t) : string =
  let ingredients_str =
    List.map
      (fun (ing, amount) ->
        let ingredient_str = Ingredient.to_string ing in
        ingredient_str ^ " - " ^ Quantity.to_string amount)
      recipe.ingredients
    |> String.concat "\n"
  in
  "Title: " ^ recipe.title ^ "\n" ^ "Servings: "
  ^ string_of_int recipe.servings
  ^ "\n" ^ "Prep Time: " ^ recipe.prep_time ^ "\n" ^ "Cook Time: "
  ^ recipe.cook_time ^ "\n" ^ "Total Time: " ^ recipe.total_time ^ "\n\n"
  ^ "Ingredients:\n" ^ ingredients_str ^ "\n\n" ^ "Instructions:\n"
  ^ recipe.instructions
