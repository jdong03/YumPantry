module type Ingredient = sig 
    (** Subtypes of ingredients. *)
    type meat
    type vegetable
    type fruit
    type dairy
    type condiments
    type spices
    type grain

    type ingredient 
    (** The general type given to all valid ingredients. *)

    val of_string : string -> ingredient option
    (** Converts a string to a value of type ingredient. *)
end

