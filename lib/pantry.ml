open Ingredient
open Quantity

type t = (Ingredient.ingredient * amount) list

let empty = []

let add (pantry : t) (ing : Ingredient.ingredient) (a : Quantity.amount) : t =
  match List.assoc_opt ing pantry with
  | Some old_a -> begin
      let pantry_without_assoc = List.remove_assoc ing pantry in
      match old_a with
      | Volume v1 -> begin
          match a with
          | Volume v2 ->
              ( ing,
                Volume (Quantity.Volume.add v1 v2 |> Quantity.Volume.simplify)
              )
              :: pantry_without_assoc
          | _ -> pantry
          end
      | Mass m1 -> begin
          match a with
          | Mass m2 ->
              (ing, Mass (Quantity.Mass.add m1 m2 |> Quantity.Mass.simplify))
              :: pantry_without_assoc
          | _ -> pantry
          end
      | Count c1 -> begin
          match a with
          | Count c2 -> (ing, Count (c1 +. c2)) :: pantry_without_assoc
          | _ -> pantry
      end
    end
  | None -> (ing, a) :: pantry

let remove (pantry : t) (ing : Ingredient.ingredient) (a : Quantity.amount) : t=
    match List.assoc_opt ing pantry with
    | Some old_a -> begin
      let pantry_without_assoc = List.remove_assoc ing pantry in
      match old_a with
      | Volume v1 -> begin
          match a with
          | Volume v2 ->
              ( ing,
                Volume (Quantity.Volume.subtract v1 v2 |> Quantity.Volume.simplify)
              )
              :: pantry_without_assoc
          | _ -> pantry
          end
      | Mass m1 -> begin
          match a with
          | Mass m2 ->
              (ing, Mass (Quantity.Mass.subtract m1 m2 |> Quantity.Mass.simplify))
              :: pantry_without_assoc
          | _ -> pantry
          end
      | Count c1 -> begin
          match a with
          | Count c2 -> (ing, Count (c1 -. c2)) :: pantry_without_assoc
          | _ -> pantry
          end
      end
    | None -> pantry

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
