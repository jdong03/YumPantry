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

  let of_string (input : string) : ingredient option =
    match String.lowercase_ascii input with 
    | "pork" -> Some (Meat {food = Pork; amount = Mass (0., Ounce)})
    | "beef" -> Some (Meat {food = Beef; amount = Mass (0., Ounce)})
    | "chicken" -> Some (Meat {food = Chicken; amount = Mass (0., Ounce)})
    | "tofu" -> Some (Meat {food = Tofu; amount = Mass (0., Ounce)})
    | "egg" -> Some (Meat {food = Egg; amount = Count 0.})
    | "turkey" -> Some (Meat {food = Turkey; amount = Mass (0., Ounce)})
    | "duck" -> Some (Meat {food = Duck; amount = Mass (0., Ounce)})
    | "lamb" -> Some (Meat {food = Lamb; amount = Mass (0., Ounce)})
    | "shrimp" -> Some (Meat {food = Shrimp; amount = Mass (0., Ounce)})

    | "broccoli" -> Some (Vegetable {food = Broccoli; amount = Mass (0., Ounce)})
    | "onion" -> Some (Vegetable {food = Onion; amount = Count 0.})
    | "potato" -> Some (Vegetable {food = Potato; amount = Count 0.})
    | "corn" -> Some (Vegetable {food = Corn; amount = Mass (0., Ounce)})
    | "carrot" -> Some (Vegetable {food = Carrot; amount = Count 0.})
    | "tomato" -> Some (Vegetable {food = Tomato; amount = Count 0.})
    | "lettuce" -> Some (Vegetable {food = Lettuce; amount = Count 0.})
    | "spinach" -> Some (Vegetable {food = Spinach; amount = Mass (0., Ounce)})
    | "pepper" -> Some (Vegetable {food = Pepper; amount = Count 0.})
    | "squash" -> Some (Vegetable {food = Squash; amount = Count 0.})
    | "celery" -> Some (Vegetable {food = Celery; amount = Count 0.})
    | "mushroom" -> Some (Vegetable {food = Mushroom; amount = Mass (0., Ounce)})

    | "apple" -> Some (Fruit {food = Apple; amount = Count 0.})
    | "orange" -> Some (Fruit {food = Orange; amount = Count 0.})
    | "banana" -> Some (Fruit {food = Banana; amount = Count 0.})
    | "pineapple" -> Some (Fruit {food = Pineapple; amount = Count 0.})
    | "grape" -> Some (Fruit {food = Grape; amount = Mass (0., Ounce)})
    | "strawberry" -> Some (Fruit {food = Strawberry; amount = Mass (0., Ounce)})
    | "watermelon" -> Some (Fruit {food = Watermelon; amount = Count 0.})
    | "blueberry" -> Some (Fruit {food = Blueberry; amount = Mass (0., Ounce)})
    | "blackberry" -> Some (Fruit {food = Blackberry; amount = Mass (0., Ounce)})
    | "raspberry" -> Some (Fruit {food = Raspberry; amount = Mass (0., Ounce)})
    | "pumpkin" -> Some (Fruit {food = Pumpkin; amount = Count 0.})
    | "avacado" -> Some (Fruit {food = Avacado; amount = Count 0.})
    | "peach" -> Some (Fruit {food = Peach; amount = Count 0.})
    | "lemon" -> Some (Fruit {food = Lemon; amount = Count 0.})
    | "lime" -> Some (Fruit {food = Lime; amount = Count 0.})

    | "milk" -> Some (Dairy {food = Milk; amount = Volume (0., Cup)})
    | "butter" -> Some (Dairy {food = Butter; amount = Volume (0., Cup)})
    | "cheese" -> Some (Dairy {food = Cheese; amount = Mass (0., Ounce)})

    | "cinnamon" -> Some (Spice {food = Cinnamon; amount = Volume (0., Cup)})
    | "cumin" -> Some (Spice {food = Cumin; amount = Volume (0., Cup)})
    | "basil" -> Some (Spice {food = Basil; amount = Volume (0., Cup)})
    | "cilantro" -> Some (Spice {food = Cilantro; amount = Volume (0., Cup)})
    | "thyme" -> Some (Spice {food = Thyme; amount = Volume (0., Cup)})
    | "rosemary" -> Some (Spice {food = Rosemary; amount = Volume (0., Cup)})
    | "ginger" -> Some (Spice {food = Ginger; amount = Volume (0., Cup)})
    | "garlic" -> Some (Spice {food = Garlic; amount = Volume (0., Cup)})
    | "salt" -> Some (Spice {food = Salt; amount = Volume (0., Cup)})
    | "black pepper"| "blackpepper" -> Some (Spice {food = BlackPepper; amount = Volume (0., Cup)})
    | "nutmeg" -> Some (Spice {food = Nutmeg; amount = Volume (0., Cup)})
    | "paprika" -> Some (Spice {food = Paprika; amount = Volume (0., Cup)})
    | "sugar" -> Some (Spice {food = Sugar; amount = Volume (0., Cup)})

    | "soy sauce" | "soysauce" -> Some (Condiment {food = SoySauce; amount = Volume (0., Cup)})
    | "ketchup" -> Some (Condiment {food = Ketchup; amount = Volume (0., Cup)})
    | "olive oil" | "oliveoil" -> Some (Condiment {food = OliveOil; amount = Volume (0., Cup)})
    | "mayo" -> Some (Condiment {food = Mayo; amount = Volume (0., Cup)})
    | "honey" -> Some (Condiment {food = Honey; amount = Volume (0., Cup)})
    | "mustard" -> Some (Condiment {food = Mustard; amount = Volume (0., Cup)})
    | "peanut butter" | "peanutbutter" -> Some (Condiment {food = PeanutButter; amount = Volume (0., Cup)})
    | "jam" -> Some (Condiment {food = Jam; amount = Volume (0., Cup)})
    | "vinegar" -> Some (Condiment {food = Vinegar; amount = Volume (0., Cup)})

    | "rice" -> Some (Grain {food = Rice; amount = Volume (0., Cup)})
    | "pasta" -> Some (Grain {food = Pasta; amount = Volume (0., Cup)})
    | "bread" -> Some (Grain {food = Bread; amount = Count 0.})

    | _ -> None

  let to_string (input : ingredient) : string = 
    match input with
      | Meat spec -> begin
        match spec.food, spec.amount with
          | Pork, Mass q -> "Pork, " ^ (Mass.to_string q)
          | Beef, Mass q -> "Beef, " ^ (Mass.to_string q)
          | Fish, Mass q -> "Fish, " ^ (Mass.to_string q)
          | Chicken, Mass q -> "Chicken, " ^ (Mass.to_string q)
          | Tofu, Mass q -> "Tofu, " ^ (Mass.to_string q)
          | Egg, Count q -> "Egg, " ^ (string_of_float q)
          | Turkey, Mass q -> "Turkey, " ^ (Mass.to_string q)
          | Duck, Mass q -> "Duck, " ^ (Mass.to_string q)
          | Lamb, Mass q -> "Lamb, " ^ (Mass.to_string q)
          | Shrimp, Mass q -> "Shrimp, " ^ (Mass.to_string q)
          | _, _ -> "Invalid Argument"
      end
      | Vegetable spec -> begin
        match spec.food, spec.amount with
        | Broccoli, Mass q -> "Broccoli, " ^ (Mass.to_string q)
        | Onion, Count q -> "Onion, " ^ (string_of_float q)
        | Potato, Count q -> "Potato, " ^ (string_of_float q)
        | Corn, Mass q -> "Corn, " ^ (Mass.to_string q)
        | Carrot, Count q -> "Carrot, " ^ (string_of_float q)
        | Tomato, Count q -> "Tomato, " ^ (string_of_float q)
        | Lettuce, Count q -> "Lettuce, " ^ (string_of_float q)
        | Spinach, Mass q -> "Spinach, " ^ (Mass.to_string q)
        | Pepper, Count q -> "Pepper, " ^ (string_of_float q)
        | Squash, Count q -> "Squash, " ^ (string_of_float q)
        | Celery, Count q -> "Celery, " ^ (string_of_float q)
        | Mushroom, Mass q -> "Mushrom, " ^ (Mass.to_string q)
        | _, _ -> "Invalid Argument"
      end
      | Fruit spec -> begin
        match spec.food, spec.amount with
        | Apple, Count q -> "Apple, " ^ (string_of_float q)
        | Orange, Count q -> "Orange, " ^ (string_of_float q)
        | Banana, Count q -> "Banana, " ^ (string_of_float q)
        | Pineapple, Count q -> "Pineapple, " ^ (string_of_float q)
        | Grape, Mass q -> "Grape, " ^ (Mass.to_string q)
        | Strawberry, Mass q -> "Strawberry, " ^ (Mass.to_string q)
        | Watermelon, Count q -> "Watermelon, " ^ (string_of_float q)
        | Blueberry, Mass q -> "Blueberry, " ^ (Mass.to_string q)
        | Blackberry, Mass q -> "Blackberry, " ^ (Mass.to_string q)
        | Raspberry, Mass q -> "Raspberry, " ^ (Mass.to_string q)
        | Pumpkin, Count q -> "Pumpkin, " ^ (string_of_float q)
        | Avacado, Count q -> "Avacado, " ^ (string_of_float q)
        | Peach, Count q -> "Peach, " ^ (string_of_float q)
        | Lemon, Count q -> "Lemon, " ^ (string_of_float q)
        | Lime, Count q -> "Lime, " ^ (string_of_float q)
        | _, _ -> "Invalid Argument"
      end
      | Dairy spec -> begin
        match spec.food, spec.amount with
        | Milk, Volume q -> "Milk, " ^ (Volume.to_string q)
        | Butter, Volume q -> "Butter, " ^ (Volume.to_string q)
        | Cheese, Mass q -> "Cheese, " ^ (Mass.to_string q)
        | _, _ -> "Invalid Argument"
      end
      | Spice spec -> begin
        match spec.food, spec.amount with
        | Cinnamon, Volume q -> "Cinnamon, " ^ (Volume.to_string q)
        | Cumin, Volume q -> "Cumin, " ^ (Volume.to_string q)
        | Basil, Volume q -> "Basil, " ^ (Volume.to_string q)
        | Cilantro, Volume q -> "Cilantro, " ^ (Volume.to_string q)
        | Thyme, Volume q -> "Thyme, " ^ (Volume.to_string q)
        | Rosemary, Volume q -> "Rosemary, " ^ (Volume.to_string q)
        | Ginger, Volume q -> "Ginger, " ^ (Volume.to_string q)
        | Garlic, Volume q -> "Garlic, " ^ (Volume.to_string q)
        | Salt, Volume q -> "Salt, " ^ (Volume.to_string q)
        | BlackPepper, Volume q -> "Black Pepper, " ^ (Volume.to_string q)
        | Nutmeg, Volume q -> "Nutmeg, " ^ (Volume.to_string q)
        | Paprika, Volume q -> "Paprika, " ^ (Volume.to_string q)
        | Sugar, Volume q -> "Sugar, " ^ (Volume.to_string q)
        | _, _ -> "Invalid Argument"
      end
      | Condiment spec -> begin
        match spec.food, spec.amount with
        | SoySauce, Volume q -> "Soy Sauce, " ^ (Volume.to_string q)
        | Ketchup, Volume q -> "Ketcup, " ^ (Volume.to_string q)
        | OliveOil, Volume q -> "Olive Oil, " ^ (Volume.to_string q)
        | Mayo, Volume q -> "Mayo, " ^ (Volume.to_string q)
        | Honey, Volume q -> "Honey, " ^ (Volume.to_string q)
        | Mustard, Volume q -> "Mustard, " ^ (Volume.to_string q)
        | PeanutButter, Volume q -> "Peanut Butter, " ^ (Volume.to_string q)
        | Jam, Volume q -> "Jam, " ^ (Volume.to_string q)
        | Vinegar, Volume q -> "Vinegar, " ^ (Volume.to_string q)
        | _, _ -> "Invalid Argument"
      end
      | Grain spec -> begin
        match spec.food, spec.amount with
        | Rice, Volume q -> "Rice, " ^ (Volume.to_string q)
        | Pasta, Volume q -> "Pasta, " ^ (Volume.to_string q)
        | Bread, Count q -> "Bread, " ^ (string_of_float q)
        | _, _ -> "Invalid Argument"
      end
      
  end
