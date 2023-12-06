open OUnit2
open Yum
open Quantity
open Ingredient

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
    (* TODO: greater than *)
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
  ]

let construct_ingredient = function
  | Some i -> i
  | None -> failwith "Could not construct ingredient"

let apple = Ingredient.of_string "Apple" |> construct_ingredient
let beef = Ingredient.of_string "Beef" |> construct_ingredient

let pantry_tests =
  [
    (*Empty pantry*)
    ("Empty Test" >:: fun _ -> assert_equal "" (Pantry.display Pantry.empty));
    ( "Empty Remove" >:: fun _ ->
      assert_equal ""
        (Pantry.empty
        |> (fun pantry ->
             Pantry.remove pantry apple
               (Quantity.of_string "1.0" |> construct_quantity))
        |> Pantry.display)
        ~printer:pp_string );
    ( "Empty Remove two ingredients" >:: fun _ ->
      assert_equal ""
        (Pantry.empty
        |> (fun pantry ->
             Pantry.remove pantry apple
               (Quantity.of_string "1.0" |> construct_quantity))
        |> (fun pantry ->
             Pantry.remove pantry beef
               (Quantity.of_string "8.0 Ounce" |> construct_quantity))
        |> Pantry.display)
        ~printer:pp_string );
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
      assert_raises (Failure "Not enough ingredients") (fun () ->
          Pantry.empty
          |> (fun pantry ->
               Pantry.add pantry apple
                 (Quantity.of_string "1.0" |> construct_quantity))
          |> (fun pantry ->
               Pantry.remove pantry apple
                 (Quantity.of_string "2.0" |> construct_quantity))
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
    (* Check Contains tests*)
    ( "Contains empty pantry" >:: fun _ ->
      assert_equal false
        ( Pantry.empty |> fun pantry ->
          Pantry.check_contains pantry apple
            (Quantity.of_string "1.0" |> construct_quantity) ) );
    ( "Contains one ingredient" >:: fun _ ->
      assert_equal true
        ( ( Pantry.empty |> fun pantry ->
            Pantry.add pantry apple
              (Quantity.of_string "1.0" |> construct_quantity) )
        |> fun pantry ->
          Pantry.check_contains pantry apple
            (Quantity.of_string "1.0" |> construct_quantity) ) );
    ( "Contains one ingredient beef" >:: fun _ ->
      assert_equal true
        ( ( Pantry.empty |> fun pantry ->
            Pantry.add pantry beef
              (Quantity.of_string "12.0 Ounce" |> construct_quantity) )
        |> fun pantry ->
          Pantry.check_contains pantry beef
            (Quantity.of_string "8.0 Ounce" |> construct_quantity) ) );
    ( "Contains for pantry with two ingredients" >:: fun _ ->
      assert_equal true
        ( ( ( Pantry.empty |> fun pantry ->
              Pantry.add pantry apple
                (Quantity.of_string "1.0" |> construct_quantity) )
          |> fun pantry ->
            Pantry.add pantry beef
              (Quantity.of_string "8.0 Ounce" |> construct_quantity) )
        |> fun pantry ->
          Pantry.check_contains pantry apple
            (Quantity.of_string "0.5" |> construct_quantity) ) );
    ( "Contains for pantry with two ingredients of same type added" >:: fun _ ->
      assert_equal true
        ( ( ( Pantry.empty |> fun pantry ->
              Pantry.add pantry apple
                (Quantity.of_string "1.0" |> construct_quantity) )
          |> fun pantry ->
            Pantry.add pantry apple
              (Quantity.of_string "1.0" |> construct_quantity) )
        |> fun pantry ->
          Pantry.check_contains pantry apple
            (Quantity.of_string "2.0" |> construct_quantity) ) );
    ( "Contains for pantry with two ingredients of same type in different \
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
          Pantry.check_contains pantry apple
            (Quantity.of_string "3.0" |> construct_quantity) ) );
    ( "Contains ingredient with multiple ingredient in pantry" >:: fun _ ->
      assert_equal false
        ( ( ( Pantry.empty |> fun pantry ->
              Pantry.add pantry apple
                (Quantity.of_string "1.0" |> construct_quantity) )
          |> fun pantry ->
            Pantry.add pantry beef
              (Quantity.of_string "8.0 Ounce" |> construct_quantity) )
        |> fun pantry ->
          Pantry.check_contains pantry apple
            (Quantity.of_string "2.0" |> construct_quantity) ) );
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
