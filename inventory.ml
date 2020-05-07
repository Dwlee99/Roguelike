open Weapon
open Armor

type t = {
  melee : weapon option; 
  ranged : weapon option;
  head_prot: armor option;
  torso_prot: armor option;
  leg_prot: armor option;
  feet_prot: armor option;
  items : string list; (* change this to item list later*)
  max_items : int;
}

let init_inv = {
  melee = None;
  ranged = None;
  head_prot = None;
  torso_prot = None;
  leg_prot = None;
  feet_prot = None;
  items = [];
  max_items = 10;
}

let equip_weapon t w = 
  match w.atk_type with
  | Melee -> { t with melee = Some w; }
  | Ranged -> { t with ranged = Some w; }

let equip_armor t a = match a.a_type with
  | Head -> {t with head_prot = Some a}
  | Torso -> {t with torso_prot = Some a}
  | Legs -> {t with leg_prot = Some a}
  | Feet -> {t with feet_prot = Some a}

exception MaxItemsReached

let add_item t i = 
  if List.length t.items < t.max_items then
    {t with items = i :: t.items}
  else
    raise MaxItemsReached


let remove_item t i =
  if List.mem i t.items then 
    {
      t with
      items = List.fold_left 
          (fun acc x -> if x = i then acc else x :: acc) 
          [] t.items;
    }
  else t