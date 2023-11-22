open OUnit2
open Yum
open Quantity

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

(* let pantry_tests =
   [
     (*Empty pantry*)
     ("Empty Test" >:: fun _ -> assert_equal "" (Pantry.display Pantry.empty));
     ( "Empty Remove" >:: fun _ ->
       assert_equal "" (Pantry.empty |> Pantry.remove Apple 1 |> Pantry.display)
     );
     (*Add tests*)
     ( "Add one ingredient" >:: fun _ ->
       assert_equal "" (Pantry.empty |> Pantry.add Apple 1 |> Pantry.display) );
     ( "Add two ingredients" >:: fun _ ->
       assert_equal ""
         (Pantry.empty |> Pantry.add Apple 1 |> Pantry.add Beef 1
        |> Pantry.display) );
     ( "Add two ingredients of same type" >:: fun _ ->
       assert_equal ""
         (Pantry.empty |> Pantry.add Apple 1 |> Pantry.add Apple 1
        |> Pantry.display) );
     ( "Add two ingredients of same type with different amounts" >:: fun _ ->
       assert_equal ""
         (Pantry.empty |> Pantry.add Apple 1 |> Pantry.add Apple 2
        |> Pantry.display) );
     (*Remove tests*)
     ( "Remove one ingredient" >:: fun _ ->
       assert_equal ""
         (Pantry.empty |> Pantry.add Apple 1 |> Pantry.remove Apple 1
        |> Pantry.display) );
   ] *)

let suite = "test suite" >::: List.flatten [ quantity_tests ]
let () = run_test_tt_main suite
