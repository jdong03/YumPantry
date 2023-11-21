(* JSON parsing utils *)

let remove_double_quotes s = String.concat "" (String.split_on_char '\"' s)
let sanitize_json_string s = String.trim s |> remove_double_quotes
let mem_to_string m = Yojson.Basic.to_string m |> sanitize_json_string
