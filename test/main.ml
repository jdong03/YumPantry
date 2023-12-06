open OUnit2
open Yum
open Quantity
open Ingredient

(* TODO Write Test Plan explaning approach to testing: what you tested, anything
    you omitted testing, and why you believe that your test suite demonstrates
    the correctness of your system

    Deductions:
   -4: The test plan is missing.
   -1: The test plan does not explain which parts of the system were automatically
    tested by OUnit vs. manually tested.
   -1: The test plan does not explain what modules were tested by OUnit and how
   test cases were developed (black box, glass box, randomized, etc.).
   -1: The test plan does not provide an argument for why the testing approach
   demonstrates the correctness of the system.


   Test Plan Explanation:

   Our testing strategy encompassed both black box and glass box testing methods
   to ensure comprehensive coverage of the system's functionality. This approach
   allowed us to test the system from an external perspective (black box) and also
   understand the internal logic (glass box) for more thorough testing.

   Modules Tested with OUnit:
   - Quantity: We developed a series of tests to verify the functionality of
   quantity conversions, comparisons, and operations like addition, subtraction,
    scaling, etc. These tests were designed to handle various edge cases and
    ensure the robustness of the quantity operations.
   - Pantry: The Pantry module was tested to ensure proper management of
   ingredients, including adding, removing, finding, and checking quantities.
   Special attention was given to edge cases like handling empty pantries or
   removing more ingredients than available.

   Test Case Development:
   - For Quantity tests, we used a combination of glass box and black box
   techniques. Glass box testing was utilized to ensure coverage of all code
   paths, especially for functions with multiple branches. Black box testing was
   employed to test the system from the user's perspective, without considering
   the internal implementation.
   - Pantry tests primarily utilized black box testing, focusing on the user's
   interaction with the pantry system, such as adding and removing ingredients,
   and checking for specific quantities.

   Omitted Tests:
   - Certain complex integration scenarios involving simultaneous interactions
   between multiple modules were not extensively tested due to time constraints.
   However, individual module tests provide a high level of confidence in the
   system's reliability.

   Justification for Correctness:
   - The extensive test cases developed for each module, covering a wide range of
   scenarios and edge cases, provide strong evidence for the system's correctness.
   The combination of black box and glass box testing methods ensures that both
   the external behavior and internal logic of the system are sound and reliable.
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
          Pantry.lookup pantry
            (apple, Quantity.of_string "1.0" |> construct_quantity) ) );
    ( "Lookup one ingredient" >:: fun _ ->
      assert_equal true
        ( ( Pantry.empty |> fun pantry ->
            Pantry.add pantry apple
              (Quantity.of_string "1.0" |> construct_quantity) )
        |> fun pantry ->
          Pantry.lookup pantry
            (apple, Quantity.of_string "1.0" |> construct_quantity) ) );
    ( "Lookup one ingredient beef" >:: fun _ ->
      assert_equal true
        ( ( Pantry.empty |> fun pantry ->
            Pantry.add pantry beef
              (Quantity.of_string "12.0 Ounce" |> construct_quantity) )
        |> fun pantry ->
          Pantry.lookup pantry
            (beef, Quantity.of_string "8.0 Ounce" |> construct_quantity) ) );
    ( "Lookup for pantry with two ingredients" >:: fun _ ->
      assert_equal true
        ( ( ( Pantry.empty |> fun pantry ->
              Pantry.add pantry apple
                (Quantity.of_string "1.0" |> construct_quantity) )
          |> fun pantry ->
            Pantry.add pantry beef
              (Quantity.of_string "8.0 Ounce" |> construct_quantity) )
        |> fun pantry ->
          Pantry.lookup pantry
            (apple, Quantity.of_string "0.5" |> construct_quantity) ) );
    ( "Lookup for pantry with two ingredients of same type added" >:: fun _ ->
      assert_equal true
        ( ( ( Pantry.empty |> fun pantry ->
              Pantry.add pantry apple
                (Quantity.of_string "1.0" |> construct_quantity) )
          |> fun pantry ->
            Pantry.add pantry apple
              (Quantity.of_string "1.0" |> construct_quantity) )
        |> fun pantry ->
          Pantry.lookup pantry
            (apple, Quantity.of_string "2.0" |> construct_quantity) ) );
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
          Pantry.lookup pantry
            (apple, Quantity.of_string "3.0" |> construct_quantity) ) );
    ( "Contains ingredient with multiple ingredient in pantry" >:: fun _ ->
      assert_equal false
        ( ( ( Pantry.empty |> fun pantry ->
              Pantry.add pantry apple
                (Quantity.of_string "1.0" |> construct_quantity) )
          |> fun pantry ->
            Pantry.add pantry beef
              (Quantity.of_string "8.0 Ounce" |> construct_quantity) )
        |> fun pantry ->
          Pantry.lookup pantry
            (apple, Quantity.of_string "2.0" |> construct_quantity) ) );
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
  ]

let suite = "test suite" >::: List.flatten [ quantity_tests; pantry_tests ]
let () = run_test_tt_main suite
