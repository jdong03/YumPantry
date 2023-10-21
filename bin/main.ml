open Pantry
open Ingredient
open Quantity

(*********** command line interface ***********)
let () = 
  print_endline "\n\nWelcome to OCamlLM.\n";
  print_endline "Welcome to Yummy!";
  print_endline "Please enter what you would like to do: \n\
    \"add ingredient\", \"remove ingredient\", \"display ingredient\", 
    \"reset pantry\", or \"cook a meal\".";
  print_string "> ";
  match read_line () with
  | "add ingredient" -> 
    print_endline "What ingredient would you like to add: \n\
      \"meat\", \"vegetable\", \"fruit\", \"dairy\", \"condiments\", \"spices\", 
      or \"grain\".";
    let ingredient = read_line () |> Ingredient.of_string in
    print_endline "How much of the ingredient do you want to add?";
    let quantity = read_line () |> int_of_string in
    Pantry.add pantry ingredient quantity
  | "remove ingredient" -> 
    print_endline "What ingredient would you like to remove: \n\
      \"meat\", \"vegetable\", \"fruit\", \"dairy\", \"condiments\", \"spices\", 
      or \"grain\".";
    let ingredient = read_line () |> Ingredient.of_string in
    print_endline "How much of the ingredient do you want to add?";
    let quantity = read_line () |> int_of_string in
    Pantry.remove pantry ingredient quantity
  | "display ingredients" ->
    Pantry.display pantry
  | "cook a meal" ->
    Pantry.show_recipe pantry
  | _ -> failwith "Invalid action"
