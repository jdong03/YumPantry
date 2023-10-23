open Ingredient
open Quantity

type amount = Volume of Volume.measure | Mass of Mass.measure | Count of float
type t = (Ingredient.ingredient * amount) list

let empty = []

let add (pantry : t) (ing : Ingredient.ingredient) (a : amount) : t =
  match List.assoc_opt ing pantry with
  | Some old_a -> (
      match old_a with
      | Volume v1 -> (
          match a with
          | Volume v2 -> (ing, Volume (Quantity.Volume.add v1 v2)) :: pantry
          | _ -> pantry)
      | Mass m1 -> (
          match a with
          | Mass m2 -> (ing, Mass (Quantity.Mass.add m1 m2)) :: pantry
          | _ -> pantry)
      | Count c1 -> (
          match a with
          | Count c2 -> (ing, Count (c1 +. c2)) :: pantry
          | _ -> pantry))
  | None -> (ing, a) :: pantry

let display pantry =
  List.fold_left
    (fun acc (ing, amount) ->
      let amount_string =
        match amount with
        | Mass m -> Quantity.Mass.to_string m
        | Volume v -> Quantity.Volume.to_string v
        | Count c -> Float.to_string c
      in
      let ingredient_string = Ingredient.to_string ing in
      "\n" ^ amount_string ^ " of " ^ ingredient_string ^ acc)
    "" pantry

let reset pantry = empty
