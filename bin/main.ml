open Yum
open Pantry
open Ingredient

(** [action pantry] is the main loop of the program. It prompts the user for
    an action and then executes that action, then recursively loops. *)
let rec action pantry=
  print_endline "Please enter what you would like to do: ";
  print_endline "\"add\", \"display\", or \"reset\".";
  print_string "> ";
  
  match read_line () with
  | "add" -> 
    
    print_endline "What food would you like to add: ";
    let ingredient = 
      match read_line () |> Ingredient.of_string with
      | None -> failwith "Invalid ingredient. Please enter a valid 
      ingredient.";
      | Some x -> x 
    in
    print_endline "How much of the food do you want to add?";
    let quantity= 
      match read_line () |> Quantity.of_string with
      | None -> failwith "Invalid ingredient. Please enter a valid ingredient.";
      | Some x -> x 
    in
    let new_pantry = Pantry.add pantry ingredient quantity in
    print_endline "Ingredient added.\n";
    action new_pantry
  
  | "display" ->
    print_endline (Pantry.display pantry);
    action pantry

  | "reset" ->
    print_endline "Pantry reset.\n";
    action (Pantry.reset pantry)
  | _ -> 
    print_endline "Invalid action. Please enter a valid action.";
    action pantry

(*********** command line interface ***********)
let () = 
  print_endline "\n\nWelcome to OCamlLM.\n";
  print_endline "Welcome to Yummy!";
  let pantry = Pantry.empty in
  action pantry
