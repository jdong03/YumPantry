open Ingredient
open Quantity

type t

val empty : t
val add : t -> Ingredient.ingredient -> Quantity.amount -> t
val display : t -> string
val reset : t -> t
