module type Measurement = sig
  type units
  type measure

  val simplify : measure -> measure
  val convert : measure -> units -> measure
  val ( + ) : measure -> measure -> measure
  val ( - ) : measure -> measure -> measure
  val ( > ) : measure -> measure -> bool
  val ( < ) : measure -> measure -> bool
  val ( == ) : measure -> measure -> bool
end

module type MeasurementType = sig
  type units
  (** Possible units *)

  val sizes : units list
  (** Ordered list of units in increasing size*)

  module UnitMap : Map.S with type key = units
  (** Map with units as keys *)

  val conversion_map : float UnitMap.t
  (** Map converting a unit to an equivalent number in the finest unit *)
end

module MakeSimpleMeasurement (M : MeasurementType) : Measurement = struct
  type units = M.units
  type measure = float * M.units

  let conversion_map = M.conversion_map

  module UnitMap = M.UnitMap

  let finest_unit = UnitMap.min_binding conversion_map

  let convert (m, units) new_units =
    let volume_in_teaspons = m *. UnitMap.find units conversion_map in
    let new_volume =
      volume_in_teaspons /. UnitMap.find new_units conversion_map
    in
    (new_volume, new_units)

  let simplify (m, units) =
    let vol_in_teaspons = m *. UnitMap.find units conversion_map in
    let filtered_map =
      let is_mult x y =
        match modf (x /. y) with remainer, _ -> remainer == 0.0
      in
      UnitMap.filter
        (fun _ value -> is_mult value vol_in_teaspons)
        conversion_map
    in
    match UnitMap.max_binding filtered_map with
    | largest_unit, _ -> convert (m, units) largest_unit

  let ( + ) (m1, units1) (m2, units2) =
    let a = UnitMap.find units1 conversion_map in
    let b = UnitMap.find units2 conversion_map in
    if a < b then
      (* vol_unit_a is 'finer' *)
      let vol_b_converted = m2 *. (b /. a) in
      simplify (m1 +. vol_b_converted, units1)
    else
      (* vol_unit_b is 'finer *)
      let vol_a_converted = m1 *. (a /. b) in
      simplify (vol_a_converted +. m2, units2)

  let ( - ) (m1, units1) (m2, units2) =
    let a = UnitMap.find units1 conversion_map in
    let b = UnitMap.find units2 conversion_map in
    if a < b then
      (* vol_unit_a is 'finer' *)
      let vol_b_converted = m2 *. (b /. a) in
      simplify (m1 -. vol_b_converted, units1)
    else
      (* vol_unit_b is 'finer *)
      let vol_a_converted = m1 *. (a /. b) in
      simplify (vol_a_converted -. m2, units2)

  let ( > ) (m1, units1) (m2, units2) =
    match (m1, units1) - (m2, units2) with
    | new_m, _ -> if new_m > 0.0 then true else false

  let ( < ) (m1, units1) (m1, units2) =
    match (m1, units2) - (m1, units2) with
    | new_m, _ -> if new_m < 0.0 then true else false

  let ( == ) (m1, units1) (m2, units2) =
    match (m1, units1) - (m2, units2) with
    | new_m, _ -> if new_m == 0.0 then true else false
end

let rec compare_units units_list a b =
  match units_list with
  | [] -> 0
  | h :: t -> (
      match h with
      | x when x == a -> -1 (*a is smaller than b*)
      | x when x == b -> 1 (*a is larger than b*)
      | _ -> compare_units t a b)

module Volume = MakeSimpleMeasurement (struct
  type units =
    | Teaspoon
    | Tablespoon
    | QuaterCup
    | HalfCup
    | Cup
    | Pint
    | Quart
    | Gallon

  let sizes =
    [ Teaspoon; Tablespoon; QuaterCup; HalfCup; Cup; Pint; Quart; Gallon ]

  (** Map with volume_unit as keys *)
  module UnitMap = Map.Make (struct
    type t = units

    (** Some sneaky stuff *)
    let compare = compare_units sizes
  end)

  (** Map converting a volume_unit to an equivalent number of teaspons *)
  let conversion_map =
    UnitMap.empty |> UnitMap.add Teaspoon 1.0 |> UnitMap.add Tablespoon 3.0
    |> UnitMap.add QuaterCup 4.0 |> UnitMap.add HalfCup 8.0
    |> UnitMap.add Cup 16.0 |> UnitMap.add Pint 32.0 |> UnitMap.add Quart 64.0
    |> UnitMap.add Gallon 256.0
end)

(* module MakeMeasurement3 (X : MeasurementType) : Measurement = struct
     (** Map with volume_unit as keys *)
     module UnitMap = Map.Make (struct
       type t = units

       let compare = compare
     end)

     type volume_unit =
       | Teaspoon
       | Tablespoon
       | QuaterCup
       | HalfCup
       | Cup
       | Pint
       | Quart
       | Gallon

     type volume = float * volume_unit

     (** Map with volume_unit as keys *)
     module UnitMap = Map.Make (struct
       type t = volume_unit

       let compare = compare
     end)

     (** Map converting a volume_unit to an equivalent number of teaspons *)
     let conversion_map =
       UnitMap.empty |> UnitMap.add Teaspoon 1.0 |> UnitMap.add Tablespoon 3.0
       |> UnitMap.add QuaterCup 4.0 |> UnitMap.add HalfCup 8.0
       |> UnitMap.add Cup 16.0 |> UnitMap.add Pint 32.0 |> UnitMap.add Quart 64.0
       |> UnitMap.add Gallon 256.0

     let finest_unit = UnitMap.min_binding conversion_map

     let convert (vol, vol_unit) new_unit =
       let volume_in_teaspons = vol *. UnitMap.find vol_unit conversion_map in
       let new_volume =
         volume_in_teaspons /. UnitMap.find new_unit conversion_map
       in
       (new_volume, new_unit)

     let simplify (vol, vol_unit) =
       let vol_in_teaspons = vol *. UnitMap.find vol_unit conversion_map in
       let filtered_map =
         let is_mult x y =
           match modf (x /. y) with remainer, _ -> remainer == 0.0
         in
         UnitMap.filter
           (fun _ value -> is_mult value vol_in_teaspons)
           conversion_map
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

     let ( - ) (vol_a, vol_unit_a) (vol_b, vol_unit_b) =
       let a = UnitMap.find vol_unit_a conversion_map in
       let b = UnitMap.find vol_unit_b conversion_map in
       if a < b then
         (* vol_unit_a is 'finer' *)
         let vol_b_converted = vol_b *. (b /. a) in
         simplify (vol_a -. vol_b_converted, vol_unit_a)
       else
         (* vol_unit_b is 'finer *)
         let vol_a_converted = vol_a *. (a /. b) in
         simplify (vol_a_converted -. vol_b, vol_unit_b)

     let ( > ) (vol_a, vol_unit_a) (vol_b, vol_unit_b) =
       match (vol_a, vol_unit_a) - (vol_b, vol_unit_b) with
       | new_vol, _ -> if new_vol > 0.0 then true else false

     let ( > ) (vol_a, vol_unit_a) (vol_b, vol_unit_b) =
       match (vol_a, vol_unit_a) - (vol_b, vol_unit_b) with
       | new_vol, _ -> if new_vol < 0.0 then true else false

     let ( == ) (vol_a, vol_unit_a) (vol_b, vol_unit_b) =
       match (vol_a, vol_unit_a) - (vol_b, vol_unit_b) with
       | new_vol, _ -> if new_vol == 0.0 then true else false
   end *)
