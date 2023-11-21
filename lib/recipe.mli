open Ingredient
open Quantity

(* Type definition for a recipe *)
type t

(** Parse a recipe from JSON *)
val recipe_of_json : Yojson.Basic.t -> t

(** Parse a list of recipes from a file given it's path *)
val recipes_from_file : string -> t list

(** Convert a recipe to a string representation *)
val to_string : t -> string

val all_recipes : t list