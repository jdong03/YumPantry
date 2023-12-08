open Pantry
open Recipe

(** Module for matching recipes with available ingredients in a pantry. *)

val can_make_recipe : Recipe.t -> Pantry.t -> bool
(** [can_make r p] is true if the ingredients in [p] can make the recipe [r]. *)

val get_all_matches : Pantry.t -> Recipe.t list -> Recipe.t list
(** [get_all_matches p rl] is a list of all the recipes in [rl] that can be made
    with the ingredients in [p]. *)

val display_all_matches : Pantry.t -> Recipe.t list -> string list
(** [display_all_matches p rl] is a string list of all the names of the recipes
    in [rl] that can be made with the ingredients in [p]. *)

val get_selected_recipe : string -> Recipe.t list -> Recipe.t option
(** [get_selected_recipe s rl] is the recipe in [rl] whose name is [s]. *)
