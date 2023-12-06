open Yum
open Pantry
open Ingredient
open Recipe
open Quantity
open Match

(** [getValidIngredient ()] prompts the user for an ingredient and returns
    that ingredient if it is valid. Otherwise, it prompts the user again. *)
let rec getValidIngredient () =
  print_endline "\nWhat food would you like to add: ";
  match read_line () |> Ingredient.of_string with
  | None ->
      print_endline "Invalid ingredient. Please try again.\n";
      getValidIngredient ()
  | Some ingredient -> ingredient

(** Print all elements in a list using the given to_string method [f]*)
let rec print_all lst f =
  match lst with
  | [] -> ()
  | h :: t ->
      print_endline (f h);
      print_all t f

(** Print all elements in a list of recipes *)
let rec print_all_string lst =
  match lst with
  | [] -> ()
  | h :: t ->
      print_endline h;
      print_all_string t

let print_all_ingredients () = print_all all_ingredients Ingredient.to_string
let print_all_recipes () = print_all all_recipes Recipe.to_string

(** [getValidQuantity ()] prompts the user for a quantity and returns
    that quantity if it is valid. Otherwise, it prompts the user again. *)
let rec getValidQuantity (ingredient : Ingredient.t) () =
  print_endline
    ("\nHow much "
    ^ Ingredient.to_string ingredient
    ^ " do you want to add? (Measured in "
    ^ Quantity.measurement_type (Ingredient.default_units ingredient)
    ^ ")");
  match read_line () |> Quantity.of_string with
  | None ->
      print_endline "Invalid quantity. Please try again.\n";
      getValidQuantity ingredient ()
  | Some x -> x

(** [getValidRecipe ()] prompts the user for a recipe and returns
    that recipe if it is valid. Otherwise, it prompts the user again. *)
let rec getValidRecipe () =
  print_endline "\nWhat recipe would you like to cook: ";
  match Match.get_selected_recipe (read_line ()) all_recipes  with
  | None ->
      print_endline "Invalid recipe. Please try again.\n";
      getValidRecipe ()
  | Some recipe -> recipe

(** [action pantry] is the main loop of the program. It prompts the user for
    an action and then executes that action, then recursively loops. *)
let rec action pantry =
  print_endline "";
  print_endline "Please enter what you would like to do: ";
  print_endline
    "\"add\", \"remove\", \"display\", \"reset\", \"ingredients\", \
     \"recipes\", \"possible recipes\", or \"quit\"";
  print_string "> ";

  match read_line () with
  | "add" ->
      let ingredient = getValidIngredient () in
      let quantity = getValidQuantity ingredient () in
      let new_pantry = Pantry.add pantry ingredient quantity in
      print_endline "Ingredient added.";
      action new_pantry
  | "remove" ->
      let ingredient = getValidIngredient () in
      let quantity = getValidQuantity ingredient () in
      let new_pantry = Pantry.remove pantry ingredient quantity in
      print_endline "Ingredient removed.";
      action new_pantry
  | "ingredients" ->
      print_endline "All ingredients:\n";
      print_all_ingredients ();
      action pantry
  | "recipes" ->
      print_endline "All recipes:\n";
      print_all_recipes ();
      action pantry
  | "display" ->
      let display_text = Pantry.display pantry in
      if display_text = "" then print_endline "Your pantry is empty.\n"
      else print_endline (display_text ^ "\n");
      action pantry
  | "reset" ->
      print_endline "Pantry reset.";
      action (Pantry.reset pantry)
  | "possible recipes" ->
        let possible_recipes = Match.display_all_matches pantry all_recipes in
        if possible_recipes = [] then
            print_endline "You don't have enough ingredients to make any recipes."
        else 
            print_endline "Possible recipes:\n";
            print_all_string possible_recipes;
            let recipe = getValidRecipe () in
            print_endline "\nHere is the recipe:\n";
            print_endline (Recipe.to_string recipe);
        action pantry
  | "quit" ->
      print_endline "Goodbye!\n";
      ()
  | _ ->
      print_endline "Invalid action.";
      action pantry

(*********** command line interface ***********)
let () =
  print_endline "\n\nWelcome to OCamlLM.\n";
  print_endline "Welcome to Yummy!";
  let pantry = Pantry.empty in
  action pantry
