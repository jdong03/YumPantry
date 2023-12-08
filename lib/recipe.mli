(** Module for managing recipes. *)

type t
(** The type representing a recipe *)

val to_string : t -> string
(** Convert a recipe to a string representation *)

val title : t -> string
(** The title of the recipe *)

val servings : t -> int
(** The number of servings the recipe makes *)

val prep_time : t -> float
(** The time it takes to prepare the recipe *)

val cook_time : t -> float
(** The cook time of the recipe *)

val instructions : t -> string
(** The instructions for the recipe *)

val ingredients : t -> (Ingredient.t * Quantity.t) list
(** The list of ingredients in their respective quantities for this recipe *)

val all_recipes : t list
(** A list of all recipes in data/recipes.recipes.json*)

val get_information_from_name : string -> string
(** Get the instructions for a recipe from its name *)
