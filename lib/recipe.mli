open Ingredient
open Quantity

(* Type definition for a recipe *)
type t

(** Convert a recipe to a string representation *)
val to_string : t -> string

(** A list of all recipes in data/recipes.recipes.json*)
val all_recipes : t list