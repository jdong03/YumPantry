open Ingredient
open Quantity

module type PantryType = sig
  type pantry
  type ing
  type food

  val empty : pantry
  val add : pantry -> ing -> amount -> pantry
  val remove : pantry -> ing -> amount -> pantry
  val display : pantry -> string
  val reset : pantry -> pantry
end

module SimplePantry (Ing : Ingredient) = struct

  type pantry = Ing.ingredient list

  type ing = Ing.ingredient

  type food = Ing.food

  let empty : pantry = []

  let rec add(p : pantry) (f : food) (n : amount) : pantry =
    match p with
    | [] -> [ingredient {food = f; amount = n}]
    | h::t -> if f = h.food then
                (ingredient {food = f; amount = (Quantity.add n (h.amount))})::t
              else
                h::add t f n

  let rec remove (p : pantry) (f : food) (n : amount) : pantry =
    match p with
    | [] -> failwith "Ingredient not found"
    | h::t -> if f = h.food then
                (ingredient {food = f; amount = (Quantity.subtract (h.amount) n)})::t
              else
                h::remove t f n

  let rec display (p : pantry) : string =
    match p with
    | [] -> ""
    | h::t -> (Ing.to_string h) ^ "\n" ^ (display t)

  let reset (p : pantry) : pantry = []

end

module Pantry = SimplePantry (Ingredient)



