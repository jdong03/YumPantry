(* JSON parsing utils *)

let remove_double_quotes s = String.concat "" (String.split_on_char '\"' s)
let sanitize_json_string s = String.trim s |> remove_double_quotes
let string_of_mem m = Yojson.Basic.to_string m |> sanitize_json_string
