open Yum
open Pantry
open Ingredient

(** [getValidIngredient ()] prompts the user for an ingredient and returns
    that ingredient if it is valid. Otherwise, it prompts the user again. *)
let rec getValidIngredient () =
  print_endline "\nWhat food would you like to add: ";
  match read_line () |> Ingredient.of_string with
  | None ->
    print_endline "Invalid ingredient. Please try again.\n";
    getValidIngredient ()
  | Some x -> x

(** [getValidQuantity ()] prompts the user for a quantity and returns
    that quantity if it is valid. Otherwise, it prompts the user again. *)
let rec getValidQuantity () =
  print_endline "\nHow much of the food do you want to add?";
  match read_line () |> Quantity.of_string with
  | None ->
    print_endline "Invalid quantity. Please try again.\n";
    getValidQuantity ()
  | Some x -> x

(** [action pantry] is the main loop of the program. It prompts the user for
    an action and then executes that action, then recursively loops. *)
let rec action pantry =
  print_endline "Please enter what you would like to do: ";
  print_endline "\"add\", \"remove\", \"display\", \"reset\", or \"quit\"";
  print_string "> ";

  match read_line () with
  | "add" ->
    let ingredient = getValidIngredient () in
    let quantity = getValidQuantity () in
    let new_pantry = Pantry.add pantry ingredient quantity in
    print_endline "Ingredient added.\n";
    action new_pantry

  | "remove" ->
    let ingredient = getValidIngredient () in
    let quantity = getValidQuantity () in
    let new_pantry = Pantry.remove pantry ingredient quantity in
    print_endline "Ingredient removed.\n";
    action new_pantry

  | "display" ->
    let display_text = Pantry.display pantry in
    if display_text = "" then print_endline "Your pantry is empty.\n"
    else print_endline (display_text ^ "\n");
    action pantry

  | "reset" ->
    print_endline "Pantry reset.\n";
    action (Pantry.reset pantry)

  | "quit" ->
    print_endline "Goodbye!\n";
    ()
  | _ ->
    print_endline "Invalid action.\n";
    action pantry

(*********** command line interface ***********)
let () = 
  print_endline "\n\nWelcome to OCamlLM.\n";
  print_endline "Welcome to Yummy!";
  let pantry = Pantry.empty in
  action pantry
