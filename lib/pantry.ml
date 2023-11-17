open Ingredient
open Quantity

type t = (Ingredient.ingredient * amount) list

let empty = []

let add (pantry : t) (ing : Ingredient.ingredient) (a : Quantity.amount) : t =
  match List.assoc_opt ing pantry with
  | Some old_a -> (
      let pantry_without_assoc = List.remove_assoc ing pantry in
      match old_a with
      | Volume v1 -> (
          match a with
          | Volume v2 ->
              ( ing,
                Volume (Quantity.Volume.add v1 v2 |> Quantity.Volume.simplify)
              )
              :: pantry_without_assoc
          | _ -> pantry)
      | Mass m1 -> (
          match a with
          | Mass m2 ->
              (ing, Mass (Quantity.Mass.add m1 m2 |> Quantity.Mass.simplify))
              :: pantry_without_assoc
          | _ -> pantry)
      | Count c1 -> (
          match a with
          | Count c2 -> (ing, Count (c1 +. c2)) :: pantry_without_assoc
          | _ -> pantry)
      | Invalid -> pantry)
  | None -> (
      match a with
      | Volume v -> (ing, Volume (Quantity.Volume.simplify v)) :: pantry
      | Mass m -> (ing, Mass (Quantity.Mass.simplify m)) :: pantry
      | Count c -> (ing, a) :: pantry
      | Invalid -> pantry)

let remove (pantry : t) (ing : Ingredient.ingredient) (a : Quantity.amount) : t
    =
  match List.assoc_opt ing pantry with
  | Some old_a -> (
      let pantry_without_assoc = List.remove_assoc ing pantry in
      match old_a with
      | Volume v1 -> (
          match a with
          | Volume v2 ->
              ( ing,
                Volume
                  (Quantity.Volume.subtract v1 v2 |> Quantity.Volume.simplify)
              )
              :: pantry_without_assoc
          | _ -> pantry)
      | Mass m1 -> (
          match a with
          | Mass m2 ->
              ( ing,
                Mass (Quantity.Mass.subtract m1 m2 |> Quantity.Mass.simplify) )
              :: pantry_without_assoc
          | _ -> pantry)
      | Count c1 -> (
          match a with
          | Count c2 -> (ing, Count (c1 -. c2)) :: pantry_without_assoc
          | _ -> pantry)
      | Invalid -> pantry)
  | None -> pantry

let display pantry =
  List.fold_left
    (fun acc (ing, amount) ->
      let amount_string =
        match amount with
        | Mass m -> Quantity.Mass.to_string m
        | Volume v -> Quantity.Volume.to_string v
        | Count c -> Float.to_string c
        | Invalid -> ""
      in
      let ingredient_string = Ingredient.to_string ing in
      "\n" ^ amount_string ^ " of " ^ ingredient_string ^ acc)
    "" pantry

let reset pantry = empty
