type t
(** The type representing an ingredient. *)

val of_json : Yojson.Basic.t -> t
(** [of_json json] returns the ingredient represented by [json]. *)

val to_string : t -> string
(** Converts a value of type ingredient to a string. *)

val of_string : string -> t option
(** [of_string s] returns [Some ing] if the Levenshtein distance 
    between s and [to_string ing] is the lowest between s and all 
    possible ingredients, and is less than 3. 
    Returns [None] otherwise. *)

val all_ingredients : t list
(** A list of every ingredient in data/ingredients *)

val default_units : t -> Quantity.units
(** [default_units ing] returns the default units for [ing]. *)
