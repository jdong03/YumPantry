open Quantity

(** The type of measurements each ingredient can have. *)
type measurement_type =
  | MMass
  | MVolume
  | MCount

type t
(** Ingredient type. *)

val correct_measurement_type : t -> measurement_type

val of_json : Yojson.Basic.t -> t

val of_string : string -> t option
(** Converts a string to a value of type ingredient. *)

val to_string : t -> string
(** Converts a value of type ingredient to a string. *)

val all_ingredients : t list
(** A list of every ingredient in data/ingredients *)

val string_of_measurement_type : measurement_type -> string

val measurement_type_of_string : string -> measurement_type
