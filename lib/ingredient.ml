open Quantity
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

  type ingredient = 
    | Meat of {food : meat; amount : amount}
    | Vegetable of {food : vegetable; amount : amount}
    | Fruit of {food : fruit; amount : amount}
    | Dairy of {food : dairy; amount : amount}
    | Spice of {food : spice; amount : amount}
    | Condiment of {food : condiment; amount : amount}
    | Grain of {food : grain; amount : amount}

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


  end
