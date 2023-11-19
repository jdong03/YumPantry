open Ingredient
open Quantity

type t = (ingredient * amount) list

let empty = []

let set_assoc (k, v) lst =
  let lst_without_assoc = List.remove_assoc k lst in
  (k, v) :: lst_without_assoc

let correct_measurement_type ing a =
  match (ing.measurement_type, a) with
  | MMass, Mass _ | MVolume, Volume _ | MCount, Count _ -> true
  | _ -> false

let add (pantry : t) (ing : ingredient) (a : amount) : t =
  if not (correct_measurement_type ing a) then
    failwith
      ("Expected an amount of type "
      ^ string_of_measurement_type ing.measurement_type)
  else
    match List.assoc_opt ing pantry with
    | Some old_a ->
        (* There already is a binding  *)
        let new_amount =
          match (old_a, a) with
          | Mass m1, Mass m2 -> Mass (Mass.add m1 m2)
          | Volume v1, Volume v2 -> Volume (Volume.add v1 v2)
          | Count c1, Count c2 -> Count (c1 +. c2)
          | _ -> failwith "This should be impossible"
        in
        set_assoc (ing, new_amount) pantry
    (* There is no binding *)
    | None -> set_assoc (ing, a) pantry

let remove (pantry : t) (ing : ingredient) (a : amount) : t =
  if not (correct_measurement_type ing a) then
    failwith
      ("Expected an amount of type "
      ^ string_of_measurement_type ing.measurement_type)
  else
    match List.assoc_opt ing pantry with
    | Some old_a ->
        (* There already is a binding s*)
        let new_amount =
          match (old_a, a) with
          | Mass m1, Mass m2 -> Mass (Mass.subtract m1 m2)
          | Volume v1, Volume v2 -> Volume (Volume.subtract v1 v2)
          | Count c1, Count c2 -> Count (c1 -. c2)
          | _ -> failwith "This should be impossible"
        in
        set_assoc (ing, new_amount) pantry
    (* There is no binding *)
    | None -> set_assoc (ing, a) pantry

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
