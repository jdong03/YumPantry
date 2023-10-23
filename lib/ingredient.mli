module type Ingredient = sig 
    (** Subtypes of ingredients. *)
    type meat
    type vegetable
    type fruit
    type dairy
    type condiments
    type spices
    type grain

    type food

    type ingredient 
    (** The general type given to all valid ingredients. *)

    val make_ingredient : food -> 'a -> ingredient
    (** [make_ingredient f a] is an ingredient with [f] as its food and [a] as its amount. *)

    val same_food : ingredient -> ingredient -> bool
    (** [same_food i1 i2] is true if [i1] and [i2] are the same food. *)

    val add : ingredient -> ingredient -> ingredient
    (** [add i1 i2] is the ingredient that results from adding [i1] and [i2]. *)

    val sub : ingredient -> ingredient -> ingredient
    (** [sub i1 i2] is the ingredient that results from subtracting [i2] from [i1]. *)

    val of_string : string -> ingredient option
    (** Converts a string to a value of type ingredient. *)

    val to_string : ingredient -> string 
    (** Converts a value of type ingredient to a string. *)
end

