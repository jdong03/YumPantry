module Pantry = struct
  
  type ingredient = ingredient

  type pantry = (ingredient*int) list

  let empty : pantry = []

  let add(p : pantry) (f : ingredient) (n : int) : pantry =
    (f,n) :: p

  let remove (p : pantry) (f : ingredient) (n : int) : pantry =
    failwith "Unimplemented"

  let display (p : pantry) : string =
    failwith "Unimplemented"

  let reset (p : pantry) : pantry =
    failwith "Unimplemented"

end



