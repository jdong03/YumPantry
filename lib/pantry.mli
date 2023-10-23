open Ingredient
open Quantity

module type PantryType = sig

  type pantry
  (** The type to represent a pantry of food*)

  type ing
  (** The type to represent an ingredient*)

  type food
  (** The type to represent a food*)

  type amount
  (** The type to represent an amount of food*)

  val empty : pantry
  (** [empty] is an empty pantry*)

  val add : pantry -> food -> amount -> pantry
  (** [add p s n] is the pantry [p] with the food [s] in the amount [n] added to it*)

  val remove : pantry -> food -> amount -> pantry
  (** [remove p s n] is the pantry [p] with the food [s] in the amount [n] removed from it*)

  val display : pantry -> string
  (** [display p] is a string representation of the pantry [p]*)

  val reset : pantry -> pantry
  (** [reset p] is the pantry [p] with all of its food removed*)
end

module SimplePantry (Ing : IngredientType) : PantryType

module Pantry : PantryType

val build_empty_pantry : Pantry.pantry
(** [build_empty_pantry] is an empty pantry*)