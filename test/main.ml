open OUnit2
open Yum
open Quantity
open Ingredient
open Match

(* 
       Test Plan Explanation:

       Our testing strategy encompassed both black box and glass box testing
       methods to ensure comprehensive coverage of the system's functionality.
       This approach allowed us to test the system from an external perspective
       (black box) and also use the internal logic (glass box) for more thorough
       testing.

       ----------------------------------------------------------------------------

       Modules Tested with OUnit:

       - Quantity: We developed a series of tests to verify the functionality of
                   quantity conversions, comparisons, and operations like addition,
                   subtraction, scaling, etc. These tests were designed to handle
                   various edge cases and ensure the robustness of the quantity
                   operations.
       - Pantry:   The Pantry module was tested to ensure proper management of
                   ingredients, including adding, removing, finding, checking
                   quantities, the number of distinct ingredients, displaying the
                   pantry, and resetting the pantry. Special attention was given to
                   edge cases like handling empty pantries or removing more
                   ingredients than available.
       - Ingredient: We created tests for the Ingredient module's of_string
                    function to verify the functionality of our autocorrecting 
                    algorithm and the correctness of matching a string to an Ingredient.

       ----------------------------------------------------------------------------

       Test Case Development:

       - Quantity: For Quantity tests, we used a combination of glass box and black
                   box techniques. Glass box testing was utilized to ensure
                   coverage of all code paths, especially for functions with
                   multiple branches. Black box testing was employed to test the
                   system from the user's perspective, without considering the
                   internal implementation.
       - Pantry:   Pantry tests primarily utilized black box testing, focusing on
                   the user's interaction with the pantry system, such as adding
                   and removing ingredients, and checking for specific quantities.
                   After completing the functions, we wrote additional tests to
                   ensure that edge cases were handled properly and that we were
                   achieving branch coverage.
       - Ingredient: Ingredient tests primarily utilized black box testing,
                     focusing on the autocorrect feature of the of_string function.
                     Although we needed to know the general autocorrect algorithm
                     used, black box testing was used from there to verify the
                     correctness of the implementation Levenshtein distance
                     calculations.

       ----------------------------------------------------------------------------

       Omitted Unit Tests:

       - Certain complex integration scenarios involving simultaneous interactions
         between multiple modules were not extensively tested due to time
         constraints. However, individual module tests provide a high level of
         confidence in the system's reliability.

       ----------------------------------------------------------------------------

       Command Line and GUI Testing:

       - In addition to OUnit tests, we manually tested the system by running the
         command line interface and GUI. This allowed us to test the system from
         the user's perspective and ensure that the system was functioning as
         expected.
       - For the CLI, direct user simulation was employed, where typical user
         commands were inputted to observe the system’s responses, alongside
         scripted test cases for automated scenario testing. This provided a
         technical perspective on user experience.
       - The GUI testing involved manual user interaction, such as clicking buttons
         and entering data, focusing on usability aspects like intuitiveness and
         responsiveness of the interface.
       - We utilized both black and glass box testing methods for the CLI and GUI.
         Black box testing was used to test the system from the user's perspective,
         without considering the internal implementation. Glass box testing was
         employed to ensure coverage of all code paths, especially for functions
         with multiple branches.

        ----------------------------------------------------------------------------

       Justification for Correctness:

       - The extensive test cases developed for each module along with the
         thourough Command Line and GUI Testing, covering a wide range of
         scenarios and edge cases, provide strong evidence for the system's
         correctness. The combination of black box and glass box testing methods
         ensures that both the external behavior and internal logic of the
         system are sound and reliable.
*)

let pp_string s = "\"" ^ s ^ "\""

let compare_quantities q1 q2 =
  match Quantity.equivalent q1 q2 with
  | Some b -> b
  | None -> failwith "Quantities are not of the same type!"

let construct_quantity = function
  | Some q -> q
  | None -> failwith "Could not construct quantity"

let quantity_tests =
  [
    (* Simplification.*)
    ( "Simplify 1" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "1.0 Tablespoon" |> construct_quantity)
        (Quantity.of_string "3.0 Teaspoon"
        |> construct_quantity |> Quantity.simplify) );
    ( "Simplify 2" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "1.0 Gallon" |> construct_quantity)
        (Quantity.of_string "16.0 Cup"
        |> construct_quantity |> Quantity.simplify) );
    ( "Simplify 3" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "1.0 Pound" |> construct_quantity)
        (Quantity.of_string "16.0 Ounce"
        |> construct_quantity |> Quantity.simplify) );
    ( "Simplify 4" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "3.0 Quarter Cup" |> construct_quantity)
        (Quantity.of_string "12.0 Tablespoon"
        |> construct_quantity |> Quantity.simplify) );
    ( "Simplify 5" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "2.0 Teaspoon" |> construct_quantity)
        (Quantity.of_string "2.0 Teaspoon"
        |> construct_quantity |> Quantity.simplify) );
    ( "Simplify 6" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "1.0 Pound" |> construct_quantity)
        (Quantity.of_string "16.0 Ounce"
        |> construct_quantity |> Quantity.simplify) );
    ( "Simplify 7" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "1.0 Pint" |> construct_quantity)
        (Quantity.of_string "0.5 Quart"
        |> construct_quantity |> Quantity.simplify) );
    (* Equality *)
    ( "Equality 1" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "1.0 Teaspoon" |> construct_quantity)
        (Quantity.of_string "1.0 Teaspoon" |> construct_quantity) );
    ( "Equality 2" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "5.5 Gallon" |> construct_quantity)
        (Quantity.of_string "5.5 Gallon" |> construct_quantity) );
    ( "Equality 3" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "3.0 Teaspoon" |> construct_quantity)
        (Quantity.of_string "1.0 Tablespoon" |> construct_quantity) );
    ( "Equality 4" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "1.0 Tablespoon" |> construct_quantity)
        (Quantity.of_string "3.0 Teaspoon" |> construct_quantity) );
    ( "Equality 5" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "2.5 Cup" |> construct_quantity)
        (Quantity.of_string "5.0 HalfCup" |> construct_quantity) );
    ( "Equality 6" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "0.5 Pint" |> construct_quantity)
        (Quantity.of_string "16.0 Tablespoon" |> construct_quantity) );
    ( "Equality 7" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "1.0 HalfCup" |> construct_quantity)
        (Quantity.of_string "0.5 Cup" |> construct_quantity) );
    ( "Equality 8" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "1.0 Gallon" |> construct_quantity)
        (Quantity.of_string "4.0 Quart" |> construct_quantity) );
    ( "Equality 9" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "2.0 Pound" |> construct_quantity)
        (Quantity.of_string "32.0 Ounce" |> construct_quantity) );
    (* Scaling *)
    ( "Scaling 1" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "5.0 Half Cup" |> construct_quantity)
        (Quantity.of_string "5.0 Half Cup"
        |> construct_quantity |> Quantity.scale 1.0) );
    ( "Scaling 2" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "0.25 Teaspoon" |> construct_quantity)
        (Quantity.of_string "1.0 Teaspoon"
        |> construct_quantity |> Quantity.scale 0.25) );
    ( "Scaling 3" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "1.0 Quart" |> construct_quantity)
        (Quantity.of_string "2.0 Quart"
        |> construct_quantity |> Quantity.scale 0.5) );
    ( "Scaling 4" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "5.0 Teaspoon" |> construct_quantity)
        (Quantity.of_string "2.5 Teaspoon"
        |> construct_quantity |> Quantity.scale 2.0) );
    ( "Scaling 5" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "1.0 Tablespoon" |> construct_quantity)
        (Quantity.of_string "0.25 Tablespoon"
        |> construct_quantity |> Quantity.scale 4.0) );
    (* Addition *)
    ( "Addition 1" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "1.0 Tablespoon" |> construct_quantity)
        (Quantity.add
           (Quantity.of_string "1.0 Teaspoon" |> construct_quantity)
           (Quantity.of_string "2.0 Teaspoon" |> construct_quantity)
        |> construct_quantity) );
    ( "Addition 2" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "0.0 Tablespoon" |> construct_quantity)
        (Quantity.add
           (Quantity.of_string "0.0 Pint" |> construct_quantity)
           (Quantity.of_string "0.0 Gallon" |> construct_quantity)
        |> construct_quantity) );
    ( "Addition 3" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "1.0 Gallons" |> construct_quantity)
        (Quantity.add
           (Quantity.of_string "3.0 Quart" |> construct_quantity)
           (Quantity.of_string "2.0 Pint" |> construct_quantity)
        |> construct_quantity) );
    ( "Addition 4" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "3.0 Pint" |> construct_quantity)
        (Quantity.add
           (Quantity.of_string "2.0 Pint" |> construct_quantity)
           (Quantity.of_string "1.0 Pint" |> construct_quantity)
        |> construct_quantity) );
    ( "Addition 5" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "7.0 Quart" |> construct_quantity)
        (Quantity.add
           (Quantity.of_string "3.0 Quart" |> construct_quantity)
           (Quantity.of_string "4.0 Quart" |> construct_quantity)
        |> construct_quantity) );
    (* Subtraction *)
    ( "Subtract 1" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "0.0 Tablespoon" |> construct_quantity)
        (Quantity.subtract
           (Quantity.of_string "1.0 Gallon" |> construct_quantity)
           (Quantity.of_string "1.0 Gallon" |> construct_quantity)
        |> construct_quantity) );
    ( "Subtract 2" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "3.0 Half Cup" |> construct_quantity)
        (Quantity.subtract
           (Quantity.of_string "2.0 Cup" |> construct_quantity)
           (Quantity.of_string "1.0 Half Cup" |> construct_quantity)
        |> construct_quantity) );
    ( "Subtract 3" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "0.5 Pint" |> construct_quantity)
        (Quantity.subtract
           (Quantity.of_string "1.0 Pint" |> construct_quantity)
           (Quantity.of_string "0.5 Pint" |> construct_quantity)
        |> construct_quantity) );
    ( "Subtract 4" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "2.0 Tablespoon" |> construct_quantity)
        (Quantity.subtract
           (Quantity.of_string "3.0 Tablespoon" |> construct_quantity)
           (Quantity.of_string "1.0 Tablespoon" |> construct_quantity)
        |> construct_quantity) );
    (* Less than *)
    ( "Less than 1" >:: fun _ ->
      assert_equal (Some true)
        (Quantity.less_than
           (Quantity.of_string "1.0 Quart" |> construct_quantity)
           (Quantity.of_string "1.0 Gallon" |> construct_quantity)) );
    ( "Less than 2" >:: fun _ ->
      assert_equal (Some false)
        (Quantity.less_than
           (Quantity.of_string "5.0 Quart" |> construct_quantity)
           (Quantity.of_string "1.0 Gallon" |> construct_quantity)) );
    ( "Less than 3" >:: fun _ ->
      assert_equal (Some true)
        (Quantity.less_than
           (Quantity.of_string "1.0 Pint" |> construct_quantity)
           (Quantity.of_string "2.0 Pint" |> construct_quantity)) );
    ( "Less than 4" >:: fun _ ->
      assert_equal (Some true)
        (Quantity.less_than
           (Quantity.of_string "4.0 Quart" |> construct_quantity)
           (Quantity.of_string "2.0 Gallon" |> construct_quantity)) );
    (* Greater than *)
    ( "Greater than 1" >:: fun _ ->
      assert_equal (Some false)
        (Quantity.greater_than
           (Quantity.of_string "1.0 Quart" |> construct_quantity)
           (Quantity.of_string "1.0 Gallon" |> construct_quantity)) );
    ( "Greater than 2" >:: fun _ ->
      assert_equal (Some true)
        (Quantity.greater_than
           (Quantity.of_string "5.0 Quart" |> construct_quantity)
           (Quantity.of_string "1.0 Gallon" |> construct_quantity)) );
    ( "Greater than 3" >:: fun _ ->
      assert_equal (Some false)
        (Quantity.greater_than
           (Quantity.of_string "1.0 Pint" |> construct_quantity)
           (Quantity.of_string "2.0 Pint" |> construct_quantity)) );
    ( "Greater than 4" >:: fun _ ->
      assert_equal (Some false)
        (Quantity.greater_than
           (Quantity.of_string "4.0 Quart" |> construct_quantity)
           (Quantity.of_string "2.0 Gallon" |> construct_quantity)) );
    ( "Greater than 5" >:: fun _ ->
      assert_equal (Some true)
        (Quantity.greater_than
           (Quantity.of_string "3.0 Gallon" |> construct_quantity)
           (Quantity.of_string "2.0 Gallon" |> construct_quantity)) );
    (* Of string *)
    ( "Case of units is irrelevant 1" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "1.0 Half Cup" |> construct_quantity)
        (Quantity.of_string "1.0 half cup" |> construct_quantity) );
    ( "Case of units is irrelevant 2" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "1.0 HaLF CuP" |> construct_quantity)
        (Quantity.of_string "1.0 half cup" |> construct_quantity) );
    ( "Units can have spaces 1" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "1.0 half cup" |> construct_quantity)
        (Quantity.of_string "1.0 halfcup" |> construct_quantity) );
    ( "Units can have spaces 2" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "1.0 half   cup" |> construct_quantity)
        (Quantity.of_string "1.0 halfcup" |> construct_quantity) );
    ( "Units can be plural 1" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "1.0 half cups" |> construct_quantity)
        (Quantity.of_string "1.0 half cup" |> construct_quantity) );
    ( "Units can be plural 2" >:: fun _ ->
      assert_equal ~cmp:compare_quantities
        (Quantity.of_string "1.0 gallon" |> construct_quantity)
        (Quantity.of_string "1.0 gallons" |> construct_quantity) );
    ( "Invalid String 1" >:: fun _ ->
      assert_equal None (Quantity.of_string "abc Teaspoon") );
    (* To String *)
    ( "To String 1" >:: fun _ ->
      assert_equal "1. Gallon"
        (Quantity.to_string
           (Quantity.of_string "1.0 Gallon" |> construct_quantity)) );
    (* Is Negative *)
    ( "Is Negative 1" >:: fun _ ->
      assert_equal true
        (Quantity.is_neg
           (Quantity.of_string "-1.0 Quart" |> construct_quantity)) );
    (* Is Zero *)
    ( "Is Zero 1" >:: fun _ ->
      assert_equal true
        (Quantity.is_zero
           (Quantity.of_string "0.0 Quart" |> construct_quantity)) );
    ( "Is Zero 2" >:: fun _ ->
      assert_equal true
        (Quantity.is_zero
           (Quantity.of_string "0.0 Gallon" |> construct_quantity)) );
    ( "Is Zero 3" >:: fun _ ->
      assert_equal true
        (Quantity.is_zero
           (Quantity.of_string "0.0 Ounce" |> construct_quantity)) );
    ( "Is Zero 4" >:: fun _ ->
      assert_equal true
        (Quantity.is_zero
           (Quantity.of_string "0.0 Pound" |> construct_quantity)) );
  ]

let construct_ingredient = function
  | Some i -> i
  | None -> failwith "Could not construct ingredient"

let apple = Ingredient.of_string "Apple" |> construct_ingredient
let beef = Ingredient.of_string "Beef" |> construct_ingredient
let ribeye = Ingredient.of_string "Ribeye" |> construct_ingredient
let baguette = Ingredient.of_string "Baguette" |> construct_ingredient
let all_recipes = Recipe.all_recipes
let fst_recipe = List.hd all_recipes

let pantry_tests =
  [
    (*Empty pantry*)
    ("Empty Test" >:: fun _ -> assert_equal "" (Pantry.display Pantry.empty));
    ( "Empty Remove" >:: fun _ ->
      assert_raises (Failure "Ingredient does not exist in pantry") (fun () ->
          Pantry.empty
          |> (fun pantry ->
               Pantry.remove pantry apple
                 (Quantity.of_string "1.0" |> construct_quantity))
          |> Pantry.display) );
    ( "Empty Remove two ingredients" >:: fun _ ->
      assert_raises (Failure "Ingredient does not exist in pantry") (fun () ->
          Pantry.empty
          |> (fun pantry ->
               Pantry.remove pantry apple
                 (Quantity.of_string "1.0" |> construct_quantity))
          |> (fun pantry ->
               Pantry.remove pantry beef
                 (Quantity.of_string "8.0 Ounce" |> construct_quantity))
          |> Pantry.display) );
    (*Add tests*)
    ( "Add one ingredient" >:: fun _ ->
      assert_equal "\n1. of Apple"
        (Pantry.empty
        |> (fun pantry ->
             Pantry.add pantry apple
               (Quantity.of_string "1.0" |> construct_quantity))
        |> Pantry.display)
        ~printer:pp_string );
    ( "Add two ingredients" >:: fun _ ->
      assert_equal "\n1. of Apple\n8. Ounces of Beef"
        (Pantry.empty
        |> (fun pantry ->
             Pantry.add pantry apple
               (Quantity.of_string "1.0" |> construct_quantity))
        |> (fun pantry ->
             Pantry.add pantry beef
               (Quantity.of_string "8.0 Ounce" |> construct_quantity))
        |> Pantry.display)
        ~printer:pp_string );
    ( "Add two ingredients of same type" >:: fun _ ->
      assert_equal "\n2. of Apple"
        (Pantry.empty
        |> (fun pantry ->
             Pantry.add pantry apple
               (Quantity.of_string "1.0" |> construct_quantity))
        |> (fun pantry ->
             Pantry.add pantry apple
               (Quantity.of_string "1.0" |> construct_quantity))
        |> Pantry.display)
        ~printer:pp_string );
    ( "Add two ingredients of same type with different amounts" >:: fun _ ->
      assert_equal "\n3. of Apple"
        (Pantry.empty
        |> (fun pantry ->
             Pantry.add pantry apple
               (Quantity.of_string "1.0" |> construct_quantity))
        |> (fun pantry ->
             Pantry.add pantry apple
               (Quantity.of_string "2.0" |> construct_quantity))
        |> Pantry.display)
        ~printer:pp_string );
    ( "Add two ingredient in alternating order" >:: fun _ ->
      assert_equal "\n8. Ounces of Beef\n2. of Apple"
        (Pantry.empty
        |> (fun pantry ->
             Pantry.add pantry apple
               (Quantity.of_string "1.0" |> construct_quantity))
        |> (fun pantry ->
             Pantry.add pantry beef
               (Quantity.of_string "8.0 Ounce" |> construct_quantity))
        |> (fun pantry ->
             Pantry.add pantry apple
               (Quantity.of_string "1.0" |> construct_quantity))
        |> Pantry.display)
        ~printer:pp_string );
    ( "Add ingredient with multiple ingredient in pantry" >:: fun _ ->
      assert_equal "\n1. of Apple\n8. Ounces of Beef\n1000. Ounces of Ribeye"
        (Pantry.empty
        |> (fun pantry ->
             Pantry.add pantry apple
               (Quantity.of_string "1.0" |> construct_quantity))
        |> (fun pantry ->
             Pantry.add pantry beef
               (Quantity.of_string "8.0 Ounce" |> construct_quantity))
        |> (fun pantry ->
             Pantry.add pantry ribeye
               (Quantity.of_string "1000.0 Ounce" |> construct_quantity))
        |> Pantry.display)
        ~printer:pp_string );
    (*Remove tests*)
    ( "Remove one ingredient" >:: fun _ ->
      assert_equal "\n2. of Apple"
        (Pantry.empty
        |> (fun pantry ->
             Pantry.add pantry apple
               (Quantity.of_string "3.0" |> construct_quantity))
        |> (fun pantry ->
             Pantry.remove pantry apple
               (Quantity.of_string "1.0" |> construct_quantity))
        |> Pantry.display)
        ~printer:pp_string );
    ( "Remove two ingredients" >:: fun _ ->
      assert_equal ""
        (Pantry.empty
        |> (fun pantry ->
             Pantry.add pantry apple
               (Quantity.of_string "1.0" |> construct_quantity))
        |> (fun pantry ->
             Pantry.add pantry beef
               (Quantity.of_string "8.0 Ounce" |> construct_quantity))
        |> (fun pantry ->
             Pantry.remove pantry apple
               (Quantity.of_string "1.0" |> construct_quantity))
        |> (fun pantry ->
             Pantry.remove pantry beef
               (Quantity.of_string "8.0 Ounce" |> construct_quantity))
        |> Pantry.display)
        ~printer:pp_string );
    ( "Remove more ingredients than pantry has" >:: fun _ ->
      assert_raises (Failure "Not enough ingredients in pantry") (fun () ->
          Pantry.empty
          |> (fun pantry ->
               Pantry.add pantry apple
                 (Quantity.of_string "1.0" |> construct_quantity))
          |> (fun pantry ->
               Pantry.remove pantry apple
                 (Quantity.of_string "2.0" |> construct_quantity))
          |> Pantry.display) );
    ( "Remove ingredient that does not exist in pantry" >:: fun _ ->
      assert_raises (Failure "Ingredient does not exist in pantry") (fun () ->
          Pantry.empty
          |> (fun pantry ->
               Pantry.add pantry apple
                 (Quantity.of_string "1.0" |> construct_quantity))
          |> (fun pantry ->
               Pantry.remove pantry beef
                 (Quantity.of_string "8.0 Ounce" |> construct_quantity))
          |> Pantry.display) );
    ( "Remove ingredient with multiple ingredient in pantry" >:: fun _ ->
      assert_equal "\n8. Ounces of Beef"
        (Pantry.empty
        |> (fun pantry ->
             Pantry.add pantry apple
               (Quantity.of_string "2.0" |> construct_quantity))
        |> (fun pantry ->
             Pantry.add pantry beef
               (Quantity.of_string "8.0 Ounce" |> construct_quantity))
        |> (fun pantry ->
             Pantry.remove pantry apple
               (Quantity.of_string "2.0" |> construct_quantity))
        |> Pantry.display)
        ~printer:pp_string );
    ( "Remove ingredient with multiple ingredient in pantry 3" >:: fun _ ->
      assert_raises (Failure "Not enough ingredients in pantry") (fun () ->
          Pantry.empty
          |> (fun pantry ->
               Pantry.add pantry apple
                 (Quantity.of_string "2.0" |> construct_quantity))
          |> (fun pantry ->
               Pantry.add pantry beef
                 (Quantity.of_string "8.0 Ounce" |> construct_quantity))
          |> (fun pantry ->
               Pantry.remove pantry apple
                 (Quantity.of_string "3.0" |> construct_quantity))
          |> Pantry.display) );
    (*Find tests*)
    ( "Find empty pantry" >:: fun _ ->
      assert_equal None (Pantry.empty |> fun pantry -> Pantry.find pantry apple)
    );
    ( "Find one ingredient" >:: fun _ ->
      assert_equal
        (Some (Quantity.of_string "1.0" |> construct_quantity))
        ( ( Pantry.empty |> fun pantry ->
            Pantry.add pantry apple
              (Quantity.of_string "1.0" |> construct_quantity) )
        |> fun pantry -> Pantry.find pantry apple ) );
    ( "Find two ingredients" >:: fun _ ->
      assert_equal
        (Some (Quantity.of_string "1.0" |> construct_quantity))
        ( ( ( Pantry.empty |> fun pantry ->
              Pantry.add pantry apple
                (Quantity.of_string "1.0" |> construct_quantity) )
          |> fun pantry ->
            Pantry.add pantry beef
              (Quantity.of_string "8.0 Ounce" |> construct_quantity) )
        |> fun pantry -> Pantry.find pantry apple ) );
    ( "Find two ingredients of same type" >:: fun _ ->
      assert_equal
        (Some (Quantity.of_string "2.0" |> construct_quantity))
        ( ( ( Pantry.empty |> fun pantry ->
              Pantry.add pantry apple
                (Quantity.of_string "1.0" |> construct_quantity) )
          |> fun pantry ->
            Pantry.add pantry apple
              (Quantity.of_string "1.0" |> construct_quantity) )
        |> fun pantry -> Pantry.find pantry apple ) );
    ( "Find two ingredients of same type with different amounts" >:: fun _ ->
      assert_equal
        (Some (Quantity.of_string "3.0" |> construct_quantity))
        ( ( ( Pantry.empty |> fun pantry ->
              Pantry.add pantry apple
                (Quantity.of_string "2.0" |> construct_quantity) )
          |> fun pantry ->
            Pantry.add pantry apple
              (Quantity.of_string "1.0" |> construct_quantity) )
        |> fun pantry -> Pantry.find pantry apple ) );
    ( "Find ingredient with multiple ingredient in pantry" >:: fun _ ->
      assert_equal
        (Some (Quantity.of_string "1.0" |> construct_quantity))
        ( ( ( Pantry.empty |> fun pantry ->
              Pantry.add pantry apple
                (Quantity.of_string "1.0" |> construct_quantity) )
          |> fun pantry ->
            Pantry.add pantry beef
              (Quantity.of_string "8.0 Ounce" |> construct_quantity) )
        |> fun pantry -> Pantry.find pantry apple ) );
    ( "Find ingredient that does not exist" >:: fun _ ->
      assert_equal None
        ( ( Pantry.empty |> fun pantry ->
            Pantry.add pantry apple
              (Quantity.of_string "1.0" |> construct_quantity) )
        |> fun pantry -> Pantry.find pantry beef ) );
    ( "Find ingredient that has been completley removed" >:: fun _ ->
      assert_equal None
        ( ( Pantry.empty |> fun pantry ->
            Pantry.add pantry apple
              (Quantity.of_string "1.0" |> construct_quantity) )
        |> fun pantry ->
          Pantry.remove pantry apple
            (Quantity.of_string "1.0" |> construct_quantity)
          |> fun pantry -> Pantry.find pantry apple ) );
    ( "Find ingredient that has been partially removed" >:: fun _ ->
      assert_equal
        (Some (Quantity.of_string "3.0" |> construct_quantity))
        ( ( Pantry.empty |> fun pantry ->
            Pantry.add pantry apple
              (Quantity.of_string "5.0" |> construct_quantity) )
        |> fun pantry ->
          Pantry.remove pantry apple
            (Quantity.of_string "2.0" |> construct_quantity)
          |> fun pantry -> Pantry.find pantry apple ) );
    (* Lookup tests*)
    ( "Lookup empty pantry" >:: fun _ ->
      assert_equal false
        ( Pantry.empty |> fun pantry ->
          Pantry.lookup pantry apple
            (Quantity.of_string "1.0" |> construct_quantity) ) );
    ( "Lookup one ingredient" >:: fun _ ->
      assert_equal true
        ( ( Pantry.empty |> fun pantry ->
            Pantry.add pantry apple
              (Quantity.of_string "1.0" |> construct_quantity) )
        |> fun pantry ->
          Pantry.lookup pantry apple
            (Quantity.of_string "1.0" |> construct_quantity) ) );
    ( "Lookup one ingredient beef" >:: fun _ ->
      assert_equal true
        ( ( Pantry.empty |> fun pantry ->
            Pantry.add pantry beef
              (Quantity.of_string "12.0 Ounce" |> construct_quantity) )
        |> fun pantry ->
          Pantry.lookup pantry beef
            (Quantity.of_string "8.0 Ounce" |> construct_quantity) ) );
    ( "Lookup for pantry with two ingredients" >:: fun _ ->
      assert_equal true
        ( ( ( Pantry.empty |> fun pantry ->
              Pantry.add pantry apple
                (Quantity.of_string "1.0" |> construct_quantity) )
          |> fun pantry ->
            Pantry.add pantry beef
              (Quantity.of_string "8.0 Ounce" |> construct_quantity) )
        |> fun pantry ->
          Pantry.lookup pantry apple
            (Quantity.of_string "0.5" |> construct_quantity) ) );
    ( "Lookup for pantry with two ingredients of same type added" >:: fun _ ->
      assert_equal true
        ( ( ( Pantry.empty |> fun pantry ->
              Pantry.add pantry apple
                (Quantity.of_string "1.0" |> construct_quantity) )
          |> fun pantry ->
            Pantry.add pantry apple
              (Quantity.of_string "1.0" |> construct_quantity) )
        |> fun pantry ->
          Pantry.lookup pantry apple
            (Quantity.of_string "2.0" |> construct_quantity) ) );
    ( "Lookup for pantry with two ingredients of same type in different \
       amounts added"
    >:: fun _ ->
      assert_equal true
        ( ( ( Pantry.empty |> fun pantry ->
              Pantry.add pantry apple
                (Quantity.of_string "2.0" |> construct_quantity) )
          |> fun pantry ->
            Pantry.add pantry apple
              (Quantity.of_string "1.0" |> construct_quantity) )
        |> fun pantry ->
          Pantry.lookup pantry apple
            (Quantity.of_string "3.0" |> construct_quantity) ) );
    ( "Lookup ingredient with multiple ingredient in pantry" >:: fun _ ->
      assert_equal false
        ( ( ( Pantry.empty |> fun pantry ->
              Pantry.add pantry apple
                (Quantity.of_string "1.0" |> construct_quantity) )
          |> fun pantry ->
            Pantry.add pantry beef
              (Quantity.of_string "8.0 Ounce" |> construct_quantity) )
        |> fun pantry ->
          Pantry.lookup pantry apple
            (Quantity.of_string "2.0" |> construct_quantity) ) );
    (* Distinct Ingredients tests *)
    ( "Distinct Ingredients empty pantry" >:: fun _ ->
      assert_equal 0 (Pantry.empty |> Pantry.distinct_ingredients) );
    ( "Distinct Ingredients one ingredient" >:: fun _ ->
      assert_equal 1
        ( Pantry.empty |> fun pantry ->
          Pantry.add pantry apple
            (Quantity.of_string "1.0" |> construct_quantity)
          |> Pantry.distinct_ingredients ) );
    ( "Distinct Ingredients two ingredients" >:: fun _ ->
      assert_equal 2
        ( ( Pantry.empty |> fun pantry ->
            Pantry.add pantry apple
              (Quantity.of_string "1.0" |> construct_quantity) )
        |> fun pantry ->
          Pantry.add pantry beef
            (Quantity.of_string "8.0 Ounce" |> construct_quantity)
          |> Pantry.distinct_ingredients ) );
    ( " Distinct Ingredients three ingredients" >:: fun _ ->
      assert_equal 3
        ( ( ( Pantry.empty |> fun pantry ->
              Pantry.add pantry apple
                (Quantity.of_string "1.0" |> construct_quantity) )
          |> fun pantry ->
            Pantry.add pantry beef
              (Quantity.of_string "8.0 Ounce" |> construct_quantity) )
        |> fun pantry ->
          Pantry.add pantry ribeye
            (Quantity.of_string "1000.0 Ounce" |> construct_quantity)
          |> Pantry.distinct_ingredients ) );
    ( "Distinct Ingredients two ingredients of same type" >:: fun _ ->
      assert_equal 1
        ( ( Pantry.empty |> fun pantry ->
            Pantry.add pantry apple
              (Quantity.of_string "1.0" |> construct_quantity) )
        |> fun pantry ->
          Pantry.add pantry apple
            (Quantity.of_string "1.0" |> construct_quantity)
          |> Pantry.distinct_ingredients ) );
    ( "Distinct Ingredients two ingredients of same type with different\n\
      \       amounts"
    >:: fun _ ->
      assert_equal 1
        ( ( Pantry.empty |> fun pantry ->
            Pantry.add pantry apple
              (Quantity.of_string "2.0" |> construct_quantity) )
        |> fun pantry ->
          Pantry.add pantry apple
            (Quantity.of_string "1.0" |> construct_quantity)
          |> Pantry.distinct_ingredients ) );
    ( "Distinct Ingredients after remove ingredient from pantry" >:: fun _ ->
      assert_equal 2
        ( ( ( Pantry.empty |> fun pantry ->
              Pantry.add pantry apple
                (Quantity.of_string "1.0" |> construct_quantity) )
          |> fun pantry ->
            Pantry.add pantry beef
              (Quantity.of_string "8.0 Ounce" |> construct_quantity) )
        |> fun pantry ->
          Pantry.add pantry ribeye
            (Quantity.of_string "1000.0 Ounce" |> construct_quantity)
          |> fun pantry ->
          Pantry.remove pantry apple
            (Quantity.of_string "1.0" |> construct_quantity)
          |> Pantry.distinct_ingredients ) );
    ( " Distinct Ingredients after remove all ingredients from pantry"
    >:: fun _ ->
      assert_equal 0
        ( ( ( Pantry.empty |> fun pantry ->
              Pantry.add pantry apple
                (Quantity.of_string "1.0" |> construct_quantity) )
          |> fun pantry ->
            Pantry.add pantry beef
              (Quantity.of_string "8.0 Ounce" |> construct_quantity) )
        |> fun pantry ->
          Pantry.add pantry ribeye
            (Quantity.of_string "1000.0 Ounce" |> construct_quantity)
          |> fun pantry ->
          Pantry.remove pantry apple
            (Quantity.of_string "1.0" |> construct_quantity)
          |> fun pantry ->
          Pantry.remove pantry beef
            (Quantity.of_string "8.0 Ounce" |> construct_quantity)
          |> fun pantry ->
          Pantry.remove pantry ribeye
            (Quantity.of_string "1000.0 Ounce" |> construct_quantity)
          |> Pantry.distinct_ingredients ) );
    (*Reset tests*)
    ( "Reset empty pantry" >:: fun _ ->
      assert_equal ""
        (Pantry.empty |> Pantry.reset |> Pantry.display)
        ~printer:pp_string );
    ( "Reset pantry with one ingredient" >:: fun _ ->
      assert_equal ""
        (Pantry.empty
        |> (fun pantry ->
             Pantry.add pantry apple
               (Quantity.of_string "1.0" |> construct_quantity))
        |> Pantry.reset |> Pantry.display)
        ~printer:pp_string );
    ( "Reset pantry with two ingredients" >:: fun _ ->
      assert_equal ""
        (Pantry.empty
        |> (fun pantry ->
             Pantry.add pantry apple
               (Quantity.of_string "1.0" |> construct_quantity))
        |> (fun pantry ->
             Pantry.add pantry beef
               (Quantity.of_string "8.0 Ounce" |> construct_quantity))
        |> Pantry.reset |> Pantry.display)
        ~printer:pp_string );
    (*Match: can_make_recipe tests*)
    ( "Cannot make recipe if pantry is empty" >:: fun _ ->
      assert_equal false (Pantry.empty |> Match.can_make_recipe fst_recipe) );
    ( "Cannot make recipe if pantry does not have enough ingredients"
    >:: fun _ ->
      assert_equal false
        (Pantry.empty
        |> (fun pantry ->
             Pantry.add pantry baguette
               (Quantity.of_string "1.0" |> construct_quantity))
        |> Match.can_make_recipe fst_recipe) );
    ( "Can make recipe if pantry has enough ingredients" >:: fun _ ->
      assert_equal true
        (Pantry.empty
        |> (fun pantry ->
             Pantry.add pantry baguette
               (Quantity.of_string "2.0" |> construct_quantity))
        |> Match.can_make_recipe fst_recipe) );
    ( "Cannot make recipe if pantry has the wrong ingredients" >:: fun _ ->
      assert_equal false
        (Pantry.empty
        |> (fun pantry ->
             Pantry.add pantry apple
               (Quantity.of_string "2.0" |> construct_quantity))
        |> Match.can_make_recipe fst_recipe) );
    (* TODO: this will change size depending on how many recipes there are...???*)
    ( "get_all_recipes gets all recipes" >:: fun _ ->
      assert_equal 5 (Recipe.all_recipes |> List.length) );
    ( "display_all_matches displays no matches if pantry is empty" >:: fun _ ->
      assert_equal [] (Match.display_all_matches Pantry.empty all_recipes) );
    ( "display_all_matches displays no matches if pantry does not have enough \
       ingredients"
    >:: fun _ ->
      assert_equal []
        (let pantry =
           Pantry.add Pantry.empty baguette
             (Quantity.of_string "1.0" |> construct_quantity)
         in
         Match.display_all_matches pantry all_recipes) );
    ( "display_all_matchess displays matches if pantry has enough ingredients"
    >:: fun _ ->
      assert_equal
        [ "Chocolate Chip Cookies" ]
        (let pantry =
           Pantry.add Pantry.empty baguette
             (Quantity.of_string "2.0" |> construct_quantity)
         in
         Match.display_all_matches pantry all_recipes) );
    ( "display_all_matches displays matches if pantry has the wrong ingredients"
    >:: fun _ ->
      assert_equal []
        (let pantry =
           Pantry.add Pantry.empty apple
             (Quantity.of_string "2.0" |> construct_quantity)
         in
         Match.display_all_matches pantry all_recipes) );
    ( "get_selected_recipe returns None if no recipe is selected" >:: fun _ ->
      assert_equal None (Match.get_selected_recipe "" all_recipes) );
    ( "get_selected_recipe returns None if recipe does not exist" >:: fun _ ->
      assert_equal None (Match.get_selected_recipe "Ratatouille" all_recipes) );
    ( "get_selected_recipe returns Some recipe if recipe exists" >:: fun _ ->
      assert_equal (Some fst_recipe)
        (Match.get_selected_recipe "Chocolate Chip Cookies" all_recipes) );
  ]

let ingredient_autocorrect_tests =
  [
    ( "to_string |> of_string yields the same ingredient that was inputted"
    >:: fun _ ->
      assert_equal (Some apple)
        (apple |> Ingredient.to_string |> Ingredient.of_string) );
    ( "of_string |> to_string yields the same string that was inputted with the \n\
      \  first letter capitalized"
    >:: fun _ ->
      assert_equal "Apple"
        ("apple" |> Ingredient.of_string |> Option.get |> Ingredient.to_string)
        ~printer:pp_string );
    ( "of_string autocorrect with Levenshtein distance of 1" >:: fun _ ->
      assert_equal "Apple"
        ("appl" |> Ingredient.of_string |> Option.get |> Ingredient.to_string)
        ~printer:pp_string );
    ( "of_string autocorrect with Levenshtein distance of 2" >:: fun _ ->
      assert_equal "Apple"
        ("appel" |> Ingredient.of_string |> Option.get |> Ingredient.to_string)
        ~printer:pp_string );
    ( "of_string autocorrect fails with Levenshtein distance of 3" >:: fun _ ->
      assert_equal None (Ingredient.of_string "ppelp") );
  ]

let suite =
  "test suite"
  >::: List.flatten
         [ quantity_tests; pantry_tests; ingredient_autocorrect_tests ]

let () = run_test_tt_main suite
