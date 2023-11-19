open Ingredient
open Quantity
open Util
open Yojson.Basic.Util
open Yojson.Basic

type recipe = {
  title : string;
  servings : int;
  prep_time : string;
  cook_time : string;
  total_time : string;
  ingredients : (ingredient * amount) list;
  instructions : string;
}

let parse_int s = int_of_string (String.trim s)

let ingredient_amount_of_json json : ingredient * amount =
  let ing =
    {
      name = json |> member "name" |> to_string |> remove_double_quotes;
      measurement_type =
        json |> member "measurement_type" |> to_string |> remove_double_quotes
        |> measurement_type_of_string;
    }
  in
  let quantity =
    json |> member "quantity" |> to_string |> remove_double_quotes
    |> Quantity.of_string
  in
  match quantity with
  | None -> failwith ("Failed to parse quantity for ingredient " ^ ing.name)
  | Some quantity -> (ing, quantity)

let recipe_of_json json : recipe =
  {
    title = json |> member "title" |> to_string |> remove_double_quotes;
    servings = json |> member "servings" |> to_int;
    prep_time = json |> member "prep_time" |> to_string |> remove_double_quotes;
    cook_time = json |> member "cook_time" |> to_string |> remove_double_quotes;
    total_time =
      json |> member "total_time" |> to_string |> remove_double_quotes;
    ingredients =
      json |> member "ingredients" |> to_list
      |> List.map ingredient_amount_of_json;
    instructions =
      json |> member "instructions" |> to_string |> remove_double_quotes;
  }

let recipes_from_file file =
  let json = Yojson.Basic.from_file file in
  match json with
  | `List lst -> List.map recipe_of_json lst
  | _ -> failwith "Expected a JSON list"

let to_string (recipe : recipe) : string =
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

let all_recipes = recipes_from_file "recipes/recipes.json"
