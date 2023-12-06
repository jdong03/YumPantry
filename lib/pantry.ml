open Ingredient
open Quantity

type t = (Ingredient.t * Quantity.t) list

let empty = []

let set_assoc (k, v) lst =
  let lst_without_assoc = List.remove_assoc k lst in
  (k, v) :: lst_without_assoc

let correct_measurement_type ing q =
  Quantity.same_type_of_units
    (Ingredient.default_units ing)
    (Quantity.units_of_quantity q)

let incorrect_measurement_warning ing =
  "Expected an amount of type "
  ^ Quantity.measurement_type (Ingredient.default_units ing)

let add (pantry : t) (ing : Ingredient.t) (q : Quantity.t) : t =
  if not (correct_measurement_type ing q) then
    failwith (incorrect_measurement_warning ing)
  else
    match List.assoc_opt ing pantry with
    | Some old_q -> (
        (* There already is a binding  *)
        match Quantity.add old_q q with
        | Some new_q -> set_assoc (ing, new_q) pantry
        | None -> failwith "This should not be possible ")
    (* There is no binding *)
    | None -> set_assoc (ing, q) pantry

let remove (pantry : t) (ing : Ingredient.t) (q : Quantity.t) : t =
  if not (correct_measurement_type ing q) then
    failwith (incorrect_measurement_warning ing)
  else
    match List.assoc_opt ing pantry with
    | Some old_q -> (
        (* There already is a binding  *)
        match Quantity.subtract old_q q with
        | Some new_q ->
            if Quantity.is_neg new_q then failwith "Not enough ingredients"
            else if Quantity.is_zero new_q then List.remove_assoc ing pantry
            else set_assoc (ing, new_q) pantry
        | None -> failwith "This should not be possible ")
    (* There is no binding *)
    | None -> pantry

let find (pantry : t) (ing : Ingredient.t) : Quantity.t option =
  List.assoc_opt ing pantry

let check_contains (pantry : t) (ing : Ingredient.t) (q : Quantity.t) : bool =
  match List.assoc_opt ing pantry with
  | Some old_q -> (
      (* There already is a binding  *)
      match Quantity.subtract old_q q with
      | Some new_q -> not (Quantity.is_neg new_q)
      | None -> failwith "This should not be possible ")
  (* There is no binding *)
  | None -> false

let display pantry =
  List.fold_left
    (fun acc (ing, amount) ->
      let amount_string = Quantity.to_string amount in
      let ingredient_string = Ingredient.to_string ing in
      "\n" ^ amount_string ^ " of " ^ ingredient_string ^ acc)
    "" pantry

let reset pantry = empty

let rec lookup (pantry : t) (ingred : Ingredient.t * Quantity.t) : bool =
  match pantry with
  | [] -> false
  | (ing, q) :: t ->
      if ing = fst ingred && q >= snd ingred then true else lookup t ingred
