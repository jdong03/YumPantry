open Yojson.Basic

type ingredient = { name : string; measurement_type : string }

val ingredient_of_json : Yojson.Basic.t -> ingredient
val print_name : ingredient -> unit
val ingredients_from_file : string -> ingredient list
