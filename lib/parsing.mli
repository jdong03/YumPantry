open Yojson.Basic

type ingredient = { name : string; measurement_type : string }

val print_name : ingredient -> unit
val ingredients_from_file : string -> ingredient list
val print_all : ingredient list -> unit
