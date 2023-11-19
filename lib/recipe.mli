open Ingredient
open Quantity

(* Type definition for a recipe *)
type recipe = {
  title : string;
  servings : int;
  prep_time : string;
  cook_time : string;
  total_time : string;
  ingredients : (ingredient * amount) list;
  instructions : string;
}

(* Function to parse an ingredient with its amount from JSON *)
val ingredient_amount_of_json : Yojson.Basic.t -> ingredient * amount

(* Function to parse a recipe from JSON *)
val recipe_of_json : Yojson.Basic.t -> recipe

(* Function to parse a list of recipes from a file *)
val recipes_from_file : string -> recipe list

val to_string : recipe -> string

val all_recipes : recipe list