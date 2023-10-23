open Ingredient
open Quantity

module type PantryType = sig
  type pantry
  type ing
  type food
  type amount

  val empty : pantry
  val add : pantry -> food -> amount -> pantry
  val remove : pantry -> food -> amount -> pantry
  val display : pantry -> string
  val reset : pantry -> pantry
end

module SimplePantry (Ing : IngredientType) = struct

  type pantry = Ing.ingredient list

  type ing = Ing.ingredient

  type food = Ing.food

  type amount = Ing.amount
  
  let empty : pantry = []

  let add (p : pantry) (f : food) (a : amount) : pantry =
    let new_ing = Ing.make_ingredient f a in
    let rec add_helper (p : pantry) (i : ing) : pantry = 
      match p with
      | [] -> [i]
      | h::t -> if Ing.same_food i h then 
                  Ing.add i h ::t
                else h::(add_helper t i) in
    add_helper p new_ing

  let remove (p : pantry) (f : food) (a : amount) : pantry =
    let new_ing = Ing.make_ingredient f a in
    let rec remove_helper (p : pantry) (i : ing) : pantry = 
      match p with
      | [] -> []
      | h::t -> if Ing.same_food i h then 
                  Ing.sub i h ::t
                else h::(remove_helper t i) in
    remove_helper p new_ing

  let rec display (p : pantry) : string =
    match p with
    | [] -> ""
    | h::t -> (Ing.to_string h) ^ "\n" ^ (display t)

  let reset (p : pantry) : pantry = []

end

module Pantry = SimplePantry (Ingredient)

  

