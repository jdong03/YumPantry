open Quantity

(** The type of measurements each ingredient can have. *)
type measurement_type =
  | MMass
  | MVolume
  | MCount

type ingredient = { name : string; measurement_type : measurement_type }
(** Record representation of an ingredient. *)

val ingredient_of_json : Yojson.Basic.t -> ingredient

val of_string : string -> ingredient option
(** Converts a string to a value of type ingredient. *)

val to_string : ingredient -> string
(** Converts a value of type ingredient to a string. *)

val all_ingredients : ingredient list
(** A list of every ingredient in data/ *)

val string_of_measurement_type : measurement_type -> string

val measurement_type_of_string : string -> measurement_type
