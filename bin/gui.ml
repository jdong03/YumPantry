open Yum
open Pantry
open Ingredient
open Quantity
open Bogue
module L = Layout
module W = Widget

(* TODO: Fix this implementation for an invalid input. Defaults to Apple. *)

(** [getValidIngredient s] matches the string s to an ingredient returns
    that ingredient if it is valid. Otherwise, it prompts the user again. *)
let rec getValidIngredient (input : string) : ingredient option =
  input |> Ingredient.of_string

(* TODO: Infinite recursion if invalid quantity. *)

(** [getValidQuantity s] matches the string s to a quantity and returns
  that quantity if it is valid. Otherwise, it prompts the user again. *)
let rec getValidQuantity (input : string) : amount option =
  input |> Quantity.of_string

let rec action_choice pantry =
  (* A shared page for adding and removing an ingredient from the pantry.*)
  let rec add_remove_input (action : string) (invalid : bool) pantry =
    let rec add_remove_function (action : string) pantry ingredient quantity =
      let ingredient = getValidIngredient ingredient in
      let quantity = getValidQuantity quantity in
      match (ingredient, quantity) with
      | None, _ | _, None -> add_remove_input action true pantry
      | Some ingredient, Some quantity ->
          let new_pantry =
            if action = "add" then Pantry.add pantry ingredient quantity
            else Pantry.remove pantry ingredient quantity
          in
          action_choice new_pantry
    in

    let invalid_label =
      if invalid = true then W.label "Invalid input. Try again." else W.label "Change Pantry" in
    let food_input = W.text_input ~prompt:"Enter a valid food" () in
    let amount_input = W.text_input ~prompt:"Enter a valid quantity" () in
    let food_label = W.label ("What food would you like to " ^ action ^ "?") in
    let amount_label = W.label ("How much would you like to " ^ action ^ "?") in
    let submit = W.button "Submit" in
    if action = "add" then
      W.on_click submit ~click:(fun _ ->
          add_remove_function action pantry (W.get_text food_input)
            (W.get_text amount_input))
    else
      W.on_click submit ~click:(fun _ ->
          add_remove_function action pantry (W.get_text food_input)
            (W.get_text amount_input));
    let row1 = L.flat_of_w [ food_label; food_input ] in
    let row2 = L.flat_of_w [ amount_label; amount_input ] in
    let layout =
      L.tower [ L.resident invalid_label; row1; row2; L.resident submit ]
    in
    let board = Bogue.of_layout layout in
    Bogue.run board
  in

  (* The page that displays the contents of the pantry. *)
  let display pantry =
    let label = W.label "Pantry Contents" in
    let display_text = Pantry.display pantry in
    let contents =
      if display_text = "" then W.label "Your pantry is empty."
      else W.text_display display_text
    in
    let layout = L.tower_of_w ~align:Draw.Center [ label; contents ] in
    let board = Bogue.of_layout layout in
    Bogue.run board
  in

  (* The main page of the program. It provides various buttons that performs various actions.
      The default page that every action returns to after completely executing. *)
  let label = W.label "What would you like to do?" in
  let add_button = W.button "Add" in
  W.on_click add_button ~click:(fun _ -> add_remove_input "add" false pantry);
  let remove_button = W.button "Remove" in
  W.on_click remove_button ~click:(fun _ ->
      add_remove_input "remove" false pantry);
  let display_button = W.button "Display" in
  W.on_click display_button ~click:(fun __ -> display pantry);
  let reset_button = W.button "Reset" in
  W.on_click reset_button ~click:(fun _ -> action_choice (Pantry.reset pantry));
  let quit_button = W.button "Quit" in
  W.on_click quit_button ~click:(fun _ -> raise Bogue.Exit);
  let layout =
    L.tower_of_w ~align:Draw.Center
      [
        label;
        add_button;
        remove_button;
        display_button;
        reset_button;
        quit_button;
      ]
  in
  let board = Bogue.of_layout layout in
  Bogue.run board

(*********** command line interface ***********)
let () =
  (* print_endline "\n\nWelcome to OCamlLM.\n";
     print_endline "Welcome to Yummy!";
     let pantry = Pantry.empty in
     action pantry *)
  let pantry = Pantry.empty in
  action_choice pantry;
  Bogue.quit ()
