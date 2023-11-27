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

val display : t -> string
(* [display p] returns a string representation of pantry [p] *)

val reset : t -> t
(* [reset p] resets pantry [p] to the empty pantry *)
