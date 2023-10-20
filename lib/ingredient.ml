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

  type spices = 
    | Cinnamon
    | Cumin
    | Basil
    | Cilantro
    | Thyme
    | Rosemary
    | Ginger
    | Garlic
    | Salt
    | Pepper
    | Nutmeg
    | Paprika
    | Sugar

  type condiments = 
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
    | Meat of meat
    | Vegetable of vegetable
    | Fruit of fruit
    | Dairy of dairy
    | Spices of spices
    | Condiments of condiments
    | Grain of grain

  let of_string (input : string) : ingredient option =
    match String.lowercase_ascii input with 
    | "pork" -> Some (Meat Pork)
    | "beef" -> Some (Meat Beef)
    | "chicken" -> Some (Meat Chicken)
    | "tofu" -> Some (Meat Tofu)
    | "egg" -> Some (Meat Egg)
    | "turkey" -> Some (Meat Turkey)
    | "duck" -> Some (Meat Duck)
    | "lamb" -> Some (Meat Lamb)
    | "shrimp" -> Some (Meat Shrimp)

    | "broccoli" -> Some (Vegetable Broccoli)
    | "onion" -> Some (Vegetable Onion)
    | "potato" -> Some (Vegetable Potato)
    | "corn" -> Some (Vegetable Corn)
    | "carrot" -> Some (Vegetable Carrot)
    | "tomato" -> Some (Vegetable Tomato)
    | "lettuce" -> Some (Vegetable Lettuce)
    | "spinach" -> Some (Vegetable Spinach)
    | "pepper" -> Some (Vegetable Pepper)
    | "squash" -> Some (Vegetable Squash)
    | "celery" -> Some (Vegetable Celery)
    | "mushroom" -> Some (Vegetable Mushroom)

    | "apple" -> Some (Fruit Apple)
    | "orange" -> Some (Fruit Orange)
    | "banana" -> Some (Fruit Banana)
    | "pineapple" -> Some (Fruit Pineapple)
    | "grape" -> Some (Fruit Grape)
    | "strawberry" -> Some (Fruit Strawberry)
    | "watermelon" -> Some (Fruit Watermelon)
    | "blueberry" -> Some (Fruit Blueberry)
    | "blackberry" -> Some (Fruit Blackberry)
    | "raspberry" -> Some (Fruit Raspberry)
    | "pumpkin" -> Some (Fruit Pumpkin)
    | "avacado" -> Some (Fruit Avacado)
    | "peach" -> Some (Fruit Peach)
    | "lemon" -> Some (Fruit Lemon)
    | "lime" -> Some (Fruit Lime)

    | "milk" -> Some (Dairy Milk)
    | "butter" -> Some (Dairy Butter)
    | "cheese" -> Some (Dairy Cheese)

    | "soy sauce" | "soysauce" -> Some (Condiments SoySauce)
    | "ketchup" -> Some (Condiments Ketchup)
    | "olive oil" | "oliveoil" -> Some (Condiments OliveOil)
    | "mayo" -> Some (Condiments Mayo)
    | "honey" -> Some (Condiments Honey)
    | "mustard" -> Some (Condiments Mustard)
    | "peanut butter" | "peanutbutter" -> Some (Condiments PeanutButter)
    | "jam" -> Some (Condiments Jam)
    | "vinegar" -> Some (Condiments Vinegar)

    | "rice" -> Some (Grain Rice)
    | "pasta" -> Some (Grain Pasta)
    | "bread" -> Some (Grain Bread)

    | _ -> None


  end
