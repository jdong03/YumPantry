open Yum
open Pantry
open Ingredient
open Bogue
module L = Layout
module W = Widget


(** [getValidIngredient ()] prompts the user for an ingredient and returns
    that ingredient if it is valid. Otherwise, it prompts the user again. *)
(** TODO: Fix this implementation for an invalid input. Defaults to Apple. *)
let rec getValidIngredient (input : string) : ingredient =
  match read_line () |> Ingredient.of_string with
  | None -> Apple
  | Some x -> x

(** [getValidQuantity ()] prompts the user for a quantity and returns
    that quantity if it is valid. Otherwise, it prompts the user again. *)
(** TODO: Infinite recursion if invalid quantity. *)
let rec getValidQuantity (input : string) =
  match input |> Quantity.of_string with
  | None -> getValidQuantity input
  | Some x -> x


let text_input () = 
  let input = W.text_input ~max_size:200 ~prompt:"Enter your name" () in
  let label = W.label ~size:40 "Hello!" in
  let layout = L.tower [L.resident ~w:400 input; L.resident ~w:400 ~h:200 label] in
  let before_display () =
    let text = W.get_text input in
    W.set_text label ("Hello " ^ text ^ "!") in
    let board = Bogue.of_layout layout in
    Bogue.run ~before_display board

let rec action_choice pantry = 
  let add_function pantry ingredient quantity = 
    let ingredient = getValidIngredient ingredient in 
    let quantity = getValidQuantity quantity in
      let new_pantry = Pantry.add pantry ingredient quantity in
      action_choice new_pantry in
  
  let remove_function pantry ingredient quantity = 
    let ingredient = getValidIngredient ingredient in 
    let quantity = getValidQuantity quantity in
      let new_pantry = Pantry.remove pantry ingredient quantity in
      action_choice new_pantry in

(** A shared page for adding and removing an ingredient from the pantry.*)
  let add_remove_input (action : string) pantry = 
    let food_input = W.text_input ~prompt: "Enter a valid food" () in 
    let amount_input = W.text_input ~prompt: "Enter a valid quantity" () in 
    let food_label = W.label ("What food would you like to " ^ action ^ "?") in 
    let amount_label = W.label ("How much would you like to " ^ action ^ "?") in
    let submit = W.button   "Submit" in
      if action = "add" then 
      W.on_click submit ~click: (fun _ -> add_function pantry (W.get_text food_input) (W.get_text amount_input))
      else 
      W.on_click submit ~click: (fun _ -> remove_function pantry (W.get_text food_input) (W.get_text amount_input));
    let row1 = L.flat_of_w [food_label; food_input] in
    let row2 = L.flat_of_w [amount_label; amount_input] in 
    let layout = L.tower [row1; row2; L.resident submit] in
    let board = Bogue.of_layout layout in Bogue.run board in

  
(** The main page of the program. It provides various buttons that performs various actions.
    The default page that every action returns to after completely executing. *)
  let label = W.label "What would you like to do?" in
  let add_button = W.button "Add" in
    W.on_click add_button ~click: (fun _ -> add_remove_input "add" pantry);
  let remove_button = W.button "Remove" in
    W.on_click remove_button ~click: (fun _ -> add_remove_input "remove" pantry);
  let display_button = W.button "Display" in
  let reset_button = W.button "Reset" in
  let quit_button = W.button "Quit" in 
  let layout = L.tower_of_w ~align:Draw.Center 
    [label; add_button; remove_button; display_button; reset_button; quit_button] in
  let board = Bogue.of_layout layout in Bogue.run board

(*********** command line interface ***********)
let () = 
  (* print_endline "\n\nWelcome to OCamlLM.\n";
  print_endline "Welcome to Yummy!";
  let pantry = Pantry.empty in
  action pantry *)
    let pantry = Pantry.empty in
    action_choice pantry