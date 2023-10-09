type volume_unit = Teaspoon | Tablespoon | Cup | Pint | Quart | Gallon
type volume = float * volume_unit

module UnitMap = Map.Make (struct
  type t = volume_unit

  let compare = compare
end)

let conversion_map =
  UnitMap.empty
  |> UnitMap.add Teaspoon 1.0 (**)
  |> UnitMap.add Tablespoon 3.0 
  |> UnitMap.add Cup 16.0 
  |> UnitMap.add Pint 32.0
  |> UnitMap.add Quart 64.0 
  |> UnitMap.add Gallon 256.0

let convert (vol, vol_unit) new_unit =
  let volume_in_teaspons = vol *. UnitMap.find vol_unit conversion_map in
  let new_volume = volume_in_teaspons /. UnitMap.find new_unit conversion_map in
  (new_volume, new_unit)

let simplify (vol, vol_unit) =
  let vol_in_teaspons = vol *. UnitMap.find vol_unit conversion_map in
  let filtered_map =
    let is_mult x y =
      match modf (x /. y) with remainer, _ -> remainer == 0.0
    in
    UnitMap.filter (fun _ value -> is_mult value vol_in_teaspons) conversion_map
  in
  match UnitMap.max_binding filtered_map with
  | largest_unit, _ -> convert (vol, vol_unit) largest_unit

let ( + ) (vol_a, vol_unit_a) (vol_b, vol_unit_b) =
  let a = UnitMap.find vol_unit_a conversion_map in
  let b = UnitMap.find vol_unit_b conversion_map in
  if a < b then
    (* vol_unit_a is 'finer' *)
    let vol_b_converted = vol_b *. (b /. a) in
    simplify (vol_a +. vol_b_converted, vol_unit_a)
  else
    (* vol_unit_b is 'finer *)
    let vol_a_converted = vol_a *. (a /. b) in
    simplify (vol_a_converted +. vol_b, vol_unit_b)
