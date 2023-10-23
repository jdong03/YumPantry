(* open Yum
open Pantry
open Ingredient

(** [action pantry] is the main loop of the program. It prompts the user for
    an action and then executes that action, then recursively loops. *)
let rec action pantry =
  print_endline "Please enter what you would like to do: ";
  print_endline "\"add ingredient\", \"remove ingredient\", 
  \"display ingredients\", \"reset pantry\", or \"cook a meal\".";
  print_string "> ";
  match read_line () with
  | "add ingredient" -> 
    print_endline "What ingredient would you like to add: ";
    let ingredient = read_line () |> Ingredient.of_string in
    print_endline "How much of the ingredient do you want to add?";
    let quantity = read_line () |> int_of_string in
    let new_pantry = Pantry.add pantry ingredient quantity in
    action new_pantry
  | "remove ingredient" -> 
    print_endline "What ingredient would you like to remove: ";
    let ingredient = read_line () |> Ingredient.of_string in
    print_endline "How much of the ingredient do you want to remove?";
    let quantity = read_line () |> int_of_string in
    let new_pantry = Pantry.remove pantry ingredient quantity in
    action new_pantry
  | "display ingredients" ->
    Pantry.display pantry;
    action pantry
  | "reset pantry" ->
    let new_pantry = Pantry.reset pantry in
    action new_pantry
  | "cook a meal" ->
    failwith "Not implemented yet"
    (** TODO: Add implementation for showing recipes *)
  | _ -> 
    print_endline "Invalid action. Please enter a valid action.";
    action pantry

(*********** command line interface ***********)
let () = 
  print_endline "\n\nWelcome to OCamlLM.\n";
  print_endline "Welcome to Yummy!";
  let pantry = build_empty_pantry in
  action pantry *)
