(* Type definition for a recipe *)
type t

val to_string : t -> string
(** Convert a recipe to a string representation *)

val title : t -> string
val servings : t -> int
val prep_time : t -> float
val cook_time : t -> float
val instructions : t -> string
val ingredients : t -> (Ingredient.t * Quantity.t) list

val all_recipes : t list
(** A list of all recipes in data/recipes.recipes.json*)
