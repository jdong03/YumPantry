(** Module with utils for parsing JASON
*)

val string_of_mem : Yojson.Basic.t -> string
(** Convert a JSON member to a string. Properly sanitizes string*)
