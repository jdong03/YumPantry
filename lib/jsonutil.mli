(** Module for transforming JSON elements into strings. *)

val string_of_mem : Yojson.Basic.t -> string
(** Convert a JSON member to a string. Properly sanitizes string.*)
