open Ingredient
open Quantity

module Pantry = struct

  type pantry = ingredient list

  let empty : pantry = []

  let rec add(p : pantry) (f : ingredient) (n : amount) : pantry =
    match p with
    | [] -> [ingredient {food = f; amount = n}]
    | h::t -> if f.food = h.food then
                (ingredient {food = f; amount = (Quantity.add n (h.amount))})::t
              else
                h::add t f n

  let rec remove (p : pantry) (f : ingredient) (n : amount) : pantry =
    match p with
    | [] -> failwith "Ingredient not found"
    | h::t -> if f.food = h.food then
                (ingredient {food = f; amount = (Quantity.subtract (h.amount) n)})::t
              else
                h::remove t f n

  let rec display (p : pantry) : string =
    match p with
    | [] -> ""
    | h::t -> (Ingredient.to_string h) ^ "\n" ^ (display t)

  let reset (p : pantry) : pantry = []

end



