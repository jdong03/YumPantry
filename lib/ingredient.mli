open Quantity

type t
(** Ingredient type. *)

val of_json : Yojson.Basic.t -> t

val of_string : string -> t option
(** Converts a string to a value of type ingredient. *)

val to_string : t -> string
(** Converts a value of type ingredient to a string. *)

val all_ingredients : t list
(** A list of every ingredient in data/ingredients *)

val default_units : t -> Quantity.units