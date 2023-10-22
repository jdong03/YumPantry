module type Pantry = sig

  type ingredient
  (** The type to represent a food*)

  type pantry
  (** The type to represent a pantry of food*)

  val add : pantry -> ingredient -> int -> pantry
  (** [add p s n] is the pantry [p] with the food [s] in the amount [n] added to it*)

  val remove : pantry -> ingredient -> int -> pantry
  (** [remove p s n] is the pantry [p] with the food [s] in the amount [n] removed from it*)

  val display : pantry -> string
  (** [display p] is a string representation of the pantry [p]*)

  val reset : pantry -> pantry
  (** [reset p] is the pantry [p] with all of its food removed*)
end