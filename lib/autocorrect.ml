open Ingredient

let calc_score s1 s2 =
let len_s1 = String.length s1 in
  let len_s2 = String.length s2 in
  let matrix = Array.make_matrix (len_s1 + 1) (len_s2 + 1) 0 in

  for i = 0 to len_s1 do
    matrix.(i).(0) <- i;
  done;

  for j = 0 to len_s2 do
    matrix.(0).(j) <- j;
  done;

  for i = 1 to len_s1 do
    for j = 1 to len_s2 do
      let cost = if s1.[i - 1] = s2.[j - 1] then 0 else 1 in
      matrix.(i).(j) <-
        min (min (matrix.(i - 1).(j) + 1) (matrix.(i).(j - 1) + 1))
            (matrix.(i - 1).(j - 1) + cost);
    done;
  done;

  matrix.(len_s1).(len_s2)

let rec match_string_helper (s : string) (lst : ingredient list) (acc : int * ingredient) : (int * ingredient) = 
  match lst with 
    | [] -> acc
    | h :: t -> begin
      let score = calc_score s (Ingredient.to_string h) in 
        match acc with 
          | (num, ing) -> 
            if score < num then match_string_helper s t (score, h) 
            else match_string_helper s t acc
      end

let match_string (s : string) : ingredient option = 
  let first_ingredient = List.hd Ingredient.all_ingredients in 
  match match_string_helper s Ingredient.all_ingredients (String.length s, first_ingredient) with 
    | (score, ing) -> if score < 3 then Some ing else None
  
