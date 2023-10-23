open Quantity

module type Ingredient = sig 
  type meat
  type vegetable
  type fruit
  type dairy
  type condiments
  type spices
  type grain
  type ingredient 

  val of_string : string -> ingredient option
  val to_string : ingredient -> string 
end

module Ingredient = struct

  (** Can further specify into subgroups
      Ex. Different cuts of chicken, different types of fish, etc. *)
  type meat = 
    | Pork
    | Beef
    | Fish
    | Chicken
    | Tofu
    | Egg
    | Turkey
    | Duck
    | Lamb
    | Shrimp

  type vegetable =
    | Broccoli 
    | Onion
    | Potato
    | Corn
    | Carrot
    | Tomato
    | Lettuce
    | Spinach
    | Pepper
    | Squash
    | Celery
    | Mushroom

  type fruit = 
    | Apple
    | Orange
    | Banana
    | Pineapple
    | Grape
    | Strawberry
    | Watermelon
    | Blueberry
    | Blackberry
    | Raspberry
    | Pumpkin
    | Avacado
    | Peach
    | Lemon
    | Lime

  type dairy =
    | Milk
    | Butter
    | Cheese
  
  type spice =
    | Cinnamon
    | Cumin
    | Basil
    | Cilantro
    | Thyme
    | Rosemary
    | Ginger
    | Garlic
    | Salt
    | BlackPepper
    | Nutmeg
    | Paprika
    | Sugar

  type condiment =
    | SoySauce 
    | Ketchup
    | OliveOil
    | Mayo
    | Honey
    | Mustard
    | PeanutButter
    | Jam
    | Vinegar

  type grain =
    | Rice
    | Pasta
    | Bread
  
  type food =
    | Meat of meat
    | Vegetable of vegetable
    | Fruit of fruit
    | Dairy of dairy
    | Spice of spice
    | Condiment of condiment
    | Grain of grain

  type ingredient = {food : food; amount : amount}

  let same_food (ing1 : ingredient) (ing2 : ingredient) : bool =
    match ing1.food, ing2.food with
    | Meat m1, Meat m2 -> m1 = m2
    | Vegetable v1, Vegetable v2 -> v1 = v2
    | Fruit f1, Fruit f2 -> f1 = f2
    | Dairy d1, Dairy d2 -> d1 = d2
    | Spice s1, Spice s2 -> s1 = s2
    | Condiment c1, Condiment c2 -> c1 = c2
    | Grain g1, Grain g2 -> g1 = g2
    | _, _ -> false
  
  let add (ing1 : ingredient) (ing2 : ingredient) : ingredient =
    match ing1.amount, ing2.amount with
    | Volume v1, Volume v2 -> {food = ing1.food; amount = Volume (Volume.add v1 v2)}
    | Mass m1, Mass m2 -> {food = ing1.food; amount = Mass (Mass.add m1 m2)}
    | Count c1, Count c2 -> {food = ing1.food; amount = Count (c1 +. c2)}
    | _, _ -> failwith "Cannot add different types of amounts" 

  let remove (ing1 : ingredient) (ing2 : ingredient) : ingredient =
    match ing1.amount, ing2.amount with
    | Volume v1, Volume v2 -> {food = ing1.food; amount = Volume (Volume.subtract v1 v2)}
    | Mass m1, Mass m2 -> {food = ing1.food; amount = Mass (Mass.subtract m1 m2)}
    | Count c1, Count c2 -> {food = ing1.food; amount = Count (c1 -. c2)}
    | _, _ -> failwith "Cannot remove different types of amounts"

  let of_string (input : string) : ingredient option =
    match String.lowercase_ascii input with 
    | "pork" -> Some ({food = Meat Pork; amount = Mass (0., Ounce)})
    | "beef" -> Some ({food = Meat Beef; amount = Mass (0., Ounce)})
    | "chicken" -> Some ({food = Meat Chicken; amount = Mass (0., Ounce)})
    | "tofu" -> Some ({food = Meat Tofu; amount = Mass (0., Ounce)})
    | "egg" -> Some ({food = Meat Egg; amount = Count 0.})
    | "turkey" -> Some ({food = Meat Turkey; amount = Mass (0., Ounce)})
    | "duck" -> Some ({food = Meat Duck; amount = Mass (0., Ounce)})
    | "lamb" -> Some ({food = Meat Lamb; amount = Mass (0., Ounce)})
    | "shrimp" -> Some ({food = Meat Shrimp; amount = Mass (0., Ounce)})

    | "broccoli" -> Some ({food = Vegetable Broccoli; amount = Mass (0., Ounce)})
    | "onion" -> Some ({food = Vegetable Onion; amount = Count 0.})
    | "potato" -> Some ({food = Vegetable Potato; amount = Count 0.})
    | "corn" -> Some ({food = Vegetable Corn; amount = Mass (0., Ounce)})
    | "carrot" -> Some ({food = Vegetable Carrot; amount = Count 0.})
    | "tomato" -> Some ({food = Vegetable Tomato; amount = Count 0.})
    | "lettuce" -> Some ({food = Vegetable Lettuce; amount = Count 0.})
    | "spinach" -> Some ({food = Vegetable Spinach; amount = Mass (0., Ounce)})
    | "pepper" -> Some ({food = Vegetable Pepper; amount = Count 0.})
    | "squash" -> Some ({food = Vegetable Squash; amount = Count 0.})
    | "celery" -> Some ({food = Vegetable Celery; amount = Count 0.})
    | "mushroom" -> Some ({food = Vegetable Mushroom; amount = Mass (0., Ounce)})

    | "apple" -> Some ({food = Fruit Apple; amount = Count 0.})
    | "orange" -> Some ({food = Fruit Orange; amount = Count 0.})
    | "banana" -> Some ({food = Fruit Banana; amount = Count 0.})
    | "pineapple" -> Some ({food = Fruit Pineapple; amount = Count 0.})
    | "grape" -> Some ({food = Fruit Grape; amount = Mass (0., Ounce)})
    | "strawberry" -> Some ({food = Fruit Strawberry; amount = Mass (0., Ounce)})
    | "watermelon" -> Some ({food = Fruit Watermelon; amount = Count 0.})
    | "blueberry" -> Some ({food = Fruit Blueberry; amount = Mass (0., Ounce)})
    | "blackberry" -> Some ({food = Fruit Blackberry; amount = Mass (0., Ounce)})
    | "raspberry" -> Some ({food = Fruit Raspberry; amount = Mass (0., Ounce)})
    | "pumpkin" -> Some ({food = Fruit Pumpkin; amount = Count 0.})
    | "avacado" -> Some ({food = Fruit Avacado; amount = Count 0.})
    | "peach" -> Some ({food = Fruit Peach; amount = Count 0.})
    | "lemon" -> Some ({food = Fruit Lemon; amount = Count 0.})
    | "lime" -> Some ({food = Fruit Lime; amount = Count 0.})

    | "milk" -> Some ({food = Dairy Milk; amount = Volume (0., Cup)})
    | "butter" -> Some ({food = Dairy Butter; amount = Volume (0., Cup)})
    | "cheese" -> Some ({food = Dairy Cheese; amount = Mass (0., Ounce)})

    | "cinnamon" -> Some ({food = Spice Cinnamon; amount = Volume (0., Cup)})
    | "cumin" -> Some ({food = Spice Cumin; amount = Volume (0., Cup)})
    | "basil" -> Some ({food = Spice Basil; amount = Volume (0., Cup)})
    | "cilantro" -> Some ({food = Spice Cilantro; amount = Volume (0., Cup)})
    | "thyme" -> Some ({food = Spice Thyme; amount = Volume (0., Cup)})
    | "rosemary" -> Some ({food = Spice Rosemary; amount = Volume (0., Cup)})
    | "ginger" -> Some ({food = Spice Ginger; amount = Volume (0., Cup)})
    | "garlic" -> Some ({food = Spice Garlic; amount = Volume (0., Cup)})
    | "salt" -> Some ({food = Spice Salt; amount = Volume (0., Cup)})
    | "black pepper"| "blackpepper" -> Some ({food = Spice BlackPepper; amount = Volume (0., Cup)})
    | "nutmeg" -> Some ({food = Spice Nutmeg; amount = Volume (0., Cup)})
    | "paprika" -> Some ({food = Spice Paprika; amount = Volume (0., Cup)})
    | "sugar" -> Some ({food = Spice Sugar; amount = Volume (0., Cup)})

    | "soy sauce" | "soysauce" -> Some ({food = Condiment SoySauce; amount = Volume (0., Cup)})
    | "ketchup" -> Some ({food = Condiment Ketchup; amount = Volume (0., Cup)})
    | "olive oil" | "oliveoil" -> Some ({food = Condiment OliveOil; amount = Volume (0., Cup)})
    | "mayo" -> Some ({food = Condiment Mayo; amount = Volume (0., Cup)})
    | "honey" -> Some ({food = Condiment Honey; amount = Volume (0., Cup)})
    | "mustard" -> Some ({food = Condiment Mustard; amount = Volume (0., Cup)})
    | "peanut butter" | "peanutbutter" -> Some ({food = Condiment PeanutButter; amount = Volume (0., Cup)})
    | "jam" -> Some ({food = Condiment Jam; amount = Volume (0., Cup)})
    | "vinegar" -> Some ({food = Condiment Vinegar; amount = Volume (0., Cup)})

    | "rice" -> Some ({food = Grain Rice; amount = Volume (0., Cup)})
    | "pasta" -> Some ({food = Grain Pasta; amount = Volume (0., Cup)})
    | "bread" -> Some ({food = Grain Bread; amount = Count 0.})

    | _ -> None

  let to_string (input : ingredient) : string = 
    match input.food, input.amount with
      | Meat Pork, Mass q -> "Pork, " ^ (Mass.to_string q)
      | Meat Beef, Mass q -> "Beef, " ^ (Mass.to_string q)
      | Meat Fish, Mass q -> "Fish, " ^ (Mass.to_string q)
      | Meat Chicken, Mass q -> "Chicken, " ^ (Mass.to_string q)
      | Meat Tofu, Mass q -> "Tofu, " ^ (Mass.to_string q)
      | Meat Egg, Count q -> "Egg, " ^ (string_of_float q)
      | Meat Turkey, Mass q -> "Turkey, " ^ (Mass.to_string q)
      | Meat Duck, Mass q -> "Duck, " ^ (Mass.to_string q)
      | Meat Lamb, Mass q -> "Lamb, " ^ (Mass.to_string q)
      | Meat Shrimp, Mass q -> "Shrimp, " ^ (Mass.to_string q)

      | Vegetable Broccoli, Mass q -> "Broccoli, " ^ (Mass.to_string q)
      | Vegetable Onion, Count q -> "Onion, " ^ (string_of_float q)
      | Vegetable Potato, Count q -> "Potato, " ^ (string_of_float q)
      | Vegetable Corn, Mass q -> "Corn, " ^ (Mass.to_string q)
      | Vegetable Carrot, Count q -> "Carrot, " ^ (string_of_float q)
      | Vegetable Tomato, Count q -> "Tomato, " ^ (string_of_float q)
      | Vegetable Lettuce, Count q -> "Lettuce, " ^ (string_of_float q)
      | Vegetable Spinach, Mass q -> "Spinach, " ^ (Mass.to_string q)
      | Vegetable Pepper, Count q -> "Pepper, " ^ (string_of_float q)
      | Vegetable Squash, Count q -> "Squash, " ^ (string_of_float q)
      | Vegetable Celery, Count q -> "Celery, " ^ (string_of_float q)
      | Vegetable Mushroom, Mass q -> "Mushrom, " ^ (Mass.to_string q)

      | Fruit Apple, Count q -> "Apple, " ^ (string_of_float q)
      | Fruit Orange, Count q -> "Orange, " ^ (string_of_float q)
      | Fruit Banana, Count q -> "Banana, " ^ (string_of_float q)
      | Fruit Pineapple, Count q -> "Pineapple, " ^ (string_of_float q)
      | Fruit Grape, Mass q -> "Grape, " ^ (Mass.to_string q)
      | Fruit Strawberry, Mass q -> "Strawberry, " ^ (Mass.to_string q)
      | Fruit Watermelon, Count q -> "Watermelon, " ^ (string_of_float q)
      | Fruit Blueberry, Mass q -> "Blueberry, " ^ (Mass.to_string q)
      | Fruit Blackberry, Mass q -> "Blackberry, " ^ (Mass.to_string q)
      | Fruit Raspberry, Mass q -> "Raspberry, " ^ (Mass.to_string q)
      | Fruit Pumpkin, Count q -> "Pumpkin, " ^ (string_of_float q)
      | Fruit Avacado, Count q -> "Avacado, " ^ (string_of_float q)
      | Fruit Peach, Count q -> "Peach, " ^ (string_of_float q)
      | Fruit Lemon, Count q -> "Lemon, " ^ (string_of_float q)
      | Fruit Lime, Count q -> "Lime, " ^ (string_of_float q)

      | Dairy Milk, Volume q -> "Milk, " ^ (Volume.to_string q)
      | Dairy Butter, Volume q -> "Butter, " ^ (Volume.to_string q)
      | Dairy Cheese, Mass q -> "Cheese, " ^ (Mass.to_string q)

      | Spice Cinnamon, Volume q -> "Cinnamon, " ^ (Volume.to_string q)
      | Spice Cumin, Volume q -> "Cumin, " ^ (Volume.to_string q)
      | Spice Basil, Volume q -> "Basil, " ^ (Volume.to_string q)
      | Spice Cilantro, Volume q -> "Cilantro, " ^ (Volume.to_string q)
      | Spice Thyme, Volume q -> "Thyme, " ^ (Volume.to_string q)
      | Spice Rosemary, Volume q -> "Rosemary, " ^ (Volume.to_string q)
      | Spice Ginger, Volume q -> "Ginger, " ^ (Volume.to_string q)
      | Spice Garlic, Volume q -> "Garlic, " ^ (Volume.to_string q)
      | Spice Salt, Volume q -> "Salt, " ^ (Volume.to_string q)
      | Spice BlackPepper, Volume q -> "Black Pepper, " ^ (Volume.to_string q)
      | Spice Nutmeg, Volume q -> "Nutmeg, " ^ (Volume.to_string q)
      | Spice Paprika, Volume q -> "Paprika, " ^ (Volume.to_string q)
      | Spice Sugar, Volume q -> "Sugar, " ^ (Volume.to_string q)

      | Condiment SoySauce, Volume q -> "Soy Sauce, " ^ (Volume.to_string q)
      | Condiment Ketchup, Volume q -> "Ketcup, " ^ (Volume.to_string q)
      | Condiment OliveOil, Volume q -> "Olive Oil, " ^ (Volume.to_string q)
      | Condiment Mayo, Volume q -> "Mayo, " ^ (Volume.to_string q)
      | Condiment Honey, Volume q -> "Honey, " ^ (Volume.to_string q)
      | Condiment Mustard, Volume q -> "Mustard, " ^ (Volume.to_string q)
      | Condiment PeanutButter, Volume q -> "Peanut Butter, " ^ (Volume.to_string q)
      | Condiment Jam, Volume q -> "Jam, " ^ (Volume.to_string q)
      | Condiment Vinegar, Volume q -> "Vinegar, " ^ (Volume.to_string q)
      
      | Grain Rice, Volume q -> "Rice, " ^ (Volume.to_string q)
      | Grain Pasta, Volume q -> "Pasta, " ^ (Volume.to_string q)
      | Grain Bread, Count q -> "Bread, " ^ (string_of_float q)
      
      | _, _ -> "Invalid Argument"
      
  end
