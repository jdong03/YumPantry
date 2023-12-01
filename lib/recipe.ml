open Yojson.Basic.Util
open Yojson.Basic
open Jsonutil

module type Recipe = sig
  type t

  val to_string : t -> string
  val title : t -> string
  val servings : t -> int
  val prep_time : t -> float
  val cook_time : t -> float
  val instructions : t -> string
  val ingredients : t -> Ingredient.t list
  val all_recipes : t list
end

type t = {
  title : string;
  servings : int;
  prep_time : float;
  cook_time : float;
  total_time : float;
  ingredients : (Ingredient.t * Quantity.t) list;
  instructions : string;
}
(** Recipe type *)

(** Parse an ingredient amount pair from JSON *)
let ing_amount_of_json json : Ingredient.t * Quantity.t =
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
    prep_time = json |> member "prep_time" |> to_float;
    cook_time = json |> member "cook_time" |> to_float;
    total_time = json |> member "total_time" |> to_float;
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
  ^ "\n" ^ "Prep Time: "
  ^ string_of_float recipe.prep_time
  ^ "\n" ^ "Cook Time: "
  ^ string_of_float recipe.cook_time
  ^ "\n" ^ "Total Time: "
  ^ string_of_float recipe.total_time
  ^ "\n\n" ^ "Ingredients:\n" ^ ingredients_str ^ "\n\n" ^ "Instructions:\n"
  ^ recipe.instructions

let title r = r.title
let servings r = r.servings
let prep_time r = r.prep_time
let cook_time r = r.cook_time
let instructions r = r.instructions
let ingredients r = r.ingredients
let get_information_from_name name =
  let rec get_information_from_name_helper name recipes =
    match recipes with
    | [] -> failwith "Recipe not found"
    | h :: t ->
        if h.title = name then 
          let string_of_ingredients = 
            List.map (fun (ing, amount) -> 
                Ingredient.to_string ing ^ " - " ^ Quantity.to_string amount) 
              h.ingredients
            |> String.concat "\n"
            in
          "Name: " ^ h.title ^ "\n" ^ 
          "Servings: " ^ string_of_int h.servings ^ "\n" ^
          "Prep Time: " ^ string_of_float h.prep_time ^ "\n" ^
          "Cook Time: " ^ string_of_float h.cook_time ^ "\n" ^
          "Ingredients: " ^ string_of_ingredients ^ "\n" ^
          "Instructions: " ^ h.instructions
        else get_information_from_name_helper name t
  in
  get_information_from_name_helper name all_recipes
