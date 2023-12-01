open Quantity
open Ingredient
open Pantry
open Recipe

let can_make_recipe (r : Recipe.t) (p : Pantry.t) : bool =
  let rec helper (ingreds : (Ingredient.t * Quantity.t) list) : bool =
    match ingreds with
    | [] -> true
    | h :: t -> if Pantry.lookup p h then true && helper t else false
  in
  helper (Recipe.ingredients r)

let get_all_matches (p : Pantry.t) (rs : Recipe.t list) : Recipe.t list =
  let rec helper (rs : Recipe.t list) (acc : Recipe.t list) : Recipe.t list =
    match rs with
    | [] -> acc
    | h :: t -> if can_make_recipe h p then helper t (h :: acc) else helper t acc
  in
  helper rs []

let display_all_matches (p : Pantry.t) (rs : Recipe.t list) : string list =
  let rec helper (rs : Recipe.t list) (acc : string list) : string list =
    match rs with
    | [] -> acc
    | h :: t -> if can_make_recipe h p then 
        helper t (Recipe.title h :: acc) else helper t acc
  in
  helper rs []

let get_selected_recipe (name : string) (rs : Recipe.t list) : Recipe.t option =
  let rec helper (rs : Recipe.t list) : Recipe.t option =
    match rs with
    | [] -> None
    | h :: t -> if Recipe.title h = name then Some h else helper t
  in
  helper rs