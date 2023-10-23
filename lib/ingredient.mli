open Quantity

type name = Apple | Beef | Cucumber

type measurement_type =
  | Mass of Quantity.Mass.measure
  | Volume of Quantity.Volume.measure
  | Count of float

val of_string : string -> name option
(** Converts a string to a value of type ingredient. *)

val to_string : name -> string
(** Converts a value of type ingredient to a string. *)

module MeasurementMap : Map.S with type key = name
