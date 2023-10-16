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

let of_string (input : string) : ingredient =
  failwith "Unimplemented"
