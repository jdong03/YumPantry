open Quantity
open Ingredient
open Pantry
open Recipe

let can_make_recipe (r : Recipe.t) (p : Pantry.t) : bool =
  (* Iterate over each ingredient and quantity pair in the recipe *)
  Recipe.ingredients r
  |> List.for_all (fun (ing, req_q) ->
         (* Check if the pantry has enough of each ingredient *)
         match Pantry.lookup p ing req_q with
         | true -> true (* Pantry has enough of this ingredient *)
         | false -> false (* Pantry does not have enough; stop checking *))

let get_all_matches (p : Pantry.t) (rs : Recipe.t list) : Recipe.t list =
  let rec helper (rs : Recipe.t list) (acc : Recipe.t list) : Recipe.t list =
    match rs with
    | [] -> acc
    | h :: t ->
        if can_make_recipe h p then helper t (h :: acc) else helper t acc
  in
  helper rs []

let display_all_matches (p : Pantry.t) (rs : Recipe.t list) : string list =
  let rec helper (rs : Recipe.t list) (acc : string list) : string list =
    match rs with
    | [] -> acc
    | h :: t ->
        if can_make_recipe h p then helper t (Recipe.title h :: acc)
        else helper t acc
  in
  helper rs []

let get_selected_recipe (name : string) (rs : Recipe.t list) : Recipe.t option =
  let rec helper (rs : Recipe.t list) : Recipe.t option =
    match rs with
    | [] -> None
    | h :: t -> if Recipe.title h = name then Some h else helper t
  in
  helper rs
