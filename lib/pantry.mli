open Ingredient
open Quantity

type amount = Volume of Volume.measure | Mass of Mass.measure | Count of float
type t

val empty : t
val add : t -> Ingredient.ingredient -> amount -> t
val display : t -> string
val reset : t -> t
