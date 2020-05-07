open Weapon
open Armor

(** This module contains all the functions that manage the inventory of the
    player. This includes their inventory, their equipped weapon(s) and any 
    armor they are wearing.*)

(** The type of the inventory. This is a record containing the fields that
    represent the inventory as a whole*)
type t

exception MaxItemsReached

(** [init_inv] is a completely empty inventory with a starting value for
    max_items set to 10*)
val init_inv : t

(** [equip_weapon t w] equips the weapon [w] into the melee or ranged
    slot of the inventory [t] depending on the attack type of [w] *)
val equip_weapon : t -> weapon -> t

(** [equip_armor t a] equips the armor piece [a] into the correct
    slot. If*)
val equip_armor : t -> armor -> t

(** [add_item t i] adds an item into the item list in t if the total
    number of items is less than 10. Otherwise, it raises a 
    MaxItemsReached exception*)
val add_item : t -> string -> t (* change string to item when type is created*)

(** [remove_item t i] removes the item i from inventory t if it exists
    in t. If i is not in inventory t, the original inventory is returned
    with no change*)
val remove_item : t -> string -> t (*change string to item when type is created*)



