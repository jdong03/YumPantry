open Pantry
open Recipe

val can_make_recipe : Recipe.t -> Pantry.t -> bool
(** [can_make p r] is true if the ingredients in [p] can make the recipe [r] *)

val get_all_matches : Pantry.t -> Recipe.t list -> Recipe.t list
(** [get_all_matches p r] is a list of all the recipes in [r] that can be made
    with the ingredients in [p] *)

val display_all_matches : Pantry.t -> Recipe.t list -> string list
(** [display_all_matches p r] is a string list of all the names of the recipes
    in [r] that can be made with the ingredients in [p] *)

val get_selected_recipe : string -> Recipe.t list -> Recipe.t option
(** [get_selected_recipe s r] is the recipe in [r] whose name is [s] *)

