open Quantity

type measurement_type =
  | Mass
  | Volume
  | Count  (** The type of measurements each ingredient can have. *)

type ingredient = { name : string; measurement_type : measurement_type }
(** Record representation of an ingredient. *)

val of_string : string -> ingredient option
(** Converts a string to a value of type ingredient. *)

val to_string : ingredient -> string
(** Converts a value of type ingredient to a string. *)

val all_ingredients : ingredient list
val string_of_measurement_type : measurement_type -> string
