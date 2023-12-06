open Ingredient
open Quantity

type t
(* The representative type of a pantry *)

val empty : t
(* The empty pantry *)

val add : t -> Ingredient.t -> Quantity.t -> t
(* [add p i a] adds [a] of ingredient [i] to pantry [p] *)

val remove : t -> Ingredient.t -> Quantity.t -> t
(* [remove p i a] removes [a] of ingredient [i] from pantry [p] *)

val find : t -> Ingredient.t -> Quantity.t option
(* [find p i] returns the quantity of ingredient [i] in pantry [p].
   Returns None if ingredient [i] does not exist in pantry*)

val check_contains : t -> Ingredient.t -> Quantity.t -> bool
(* [check_contains p i a] returns true if pantry [p] contains [a] of
   ingredient [i] *)

val display : t -> string
(* [display p] returns a string representation of pantry [p] *)

val reset : t -> t
(* [reset p] resets pantry [p] to the empty pantry *)

val lookup : t -> Ingredient.t * Quantity.t -> bool
(* [lookup p (i, a)] returns true if [p] contains [a] of ingredient [i] *)
