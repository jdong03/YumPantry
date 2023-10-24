open Quantity

type ingredient =
  | Apple
  | Beef
  | Cucumber
  | Milk
  | Corn
  | Basil
  | OliveOil
  | Salt

type measurement_type = Mass | Volume | Count

val of_string : string -> ingredient option
(** Converts a string to a value of type ingredient. *)

val to_string : ingredient -> string
(** Converts a value of type ingredient to a string. *)

val correct_measurement_type : ingredient -> measurement_type
