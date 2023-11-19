let remove_double_quotes s = String.concat "" (String.split_on_char '\"' s)
