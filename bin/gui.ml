open Yum
open Pantry
open Ingredient
open Quantity
open Bogue
module L = Layout
module W = Widget

(* The main page of the program. It provides various buttons that performs various actions.
  The default page that every action returns to after completely executing. Contains 
  nested code for all pages within the GUI. *)
  let rec action_choice pantry =
  (* A shared page for adding and removing an ingredient from the pantry.*)
  let rec add_remove_input (action : string) (invalid : bool) pantry =
    let rec add_remove_function (action : string) pantry ingredient quantity =
      let ingredient = Ingredient.of_string ingredient in
      let quantity = Quantity.of_string quantity in
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
      if invalid = true then W.label "Invalid input. Try again."
      else W.label "Change Pantry"
    in
    let food_input = W.text_input ~prompt:"Enter a valid food" () in
    let amount_input = W.text_input ~prompt:"Enter a valid quantity" () in
    let food_label = W.label ("What food would you like to " ^ action ^ "?") in
    let amount_label = W.label ("How much would you like to " ^ action ^ "?") in
    let submit = W.button "Submit" in
    let quit_button = W.button "Quit" in
    let home_button = W.button "Home" in 
    let nav_buttons = L.flat_of_w [submit; home_button; quit_button] in
    let row1 = L.flat_of_w [ food_label; food_input ] in
    let row2 = L.flat_of_w [ amount_label; amount_input ] in
    let layout =
      L.tower [ L.resident invalid_label; row1; row2; nav_buttons ]
    in
    if action = "add" then
      W.on_click submit ~click:(fun _ ->
        L.hide_window layout; add_remove_function action pantry 
          (W.get_text food_input) (W.get_text amount_input); raise Bogue.Exit)
    else
      W.on_click submit ~click:(fun _ ->
        L.hide_window layout; add_remove_function action pantry 
            (W.get_text food_input) (W.get_text amount_input); raise Bogue.Exit);
    W.on_click quit_button ~click:(fun _ -> raise Bogue.Exit);
    W.on_click home_button ~click: (fun _ -> L.hide_window layout; action_choice pantry; raise Bogue.Exit);
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
    let quit_button = W.button "Quit" in
    let home_button = W.button "Home" in 
    let nav_buttons = L.flat_of_w [home_button; quit_button] in
    let layout = L.tower ~align:Draw.Center [ L.resident label; L.resident contents; nav_buttons] in
    W.on_click quit_button ~click:(fun _ -> raise Bogue.Exit);
    W.on_click home_button ~click: (fun _ -> L.hide_window layout; action_choice pantry; raise Bogue.Exit;);
    let board = Bogue.of_layout layout in
    Bogue.run board
  in

  let label = W.label "What would you like to do?" in
  let add_button = W.button "Add" in
  let remove_button = W.button "Remove" in
  let display_button = W.button "Display" in
  let reset_button = W.button "Reset" in
  let quit_button = W.button "Quit" in
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
  W.on_click add_button ~click:(fun _ -> L.hide_window layout; add_remove_input "add" false pantry; raise Bogue.Exit);
  W.on_click remove_button ~click:(fun _ ->
    L.hide_window layout; add_remove_input "remove" false pantry; raise Bogue.Exit);
  W.on_click display_button ~click:(fun __ -> L.hide_window layout; display pantry; raise Bogue.Exit);
  W.on_click reset_button ~click:(fun _ -> action_choice (Pantry.reset pantry));
  W.on_click quit_button ~click:(fun _ -> raise Bogue.Exit);
  let board = Bogue.of_layout layout in
  Bogue.run board

(*********** command line interface ***********)
let () =
  let pantry = Pantry.empty in
  action_choice pantry;
  Bogue.quit ()
