open Weapon
open Armor

type t = {
  melee : weapon option; 
  ranged : weapon option;
  armor : armor option;
  items : string list; (* change this to item list later*)
  max_items : int;
}

let init_inv = {
  melee = Some (Short_sword.Short_Sword.create_weapon 3);
  ranged = None;
  armor = None;
  items = [];
  max_items = 10;
}

let equip_weapon t w = 
  match w.atk_type with
  | Melee -> { t with melee = Some w; }
  | Ranged -> { t with ranged = Some w; }

let equip_armor t a = {t with armor = Some a}

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

let get_melee_name t = match t.melee with None -> "None" | Some x -> x.name

let get_ranged_name t = match t.ranged with None -> "None" | Some x -> x.name

let get_armor_name t = match t.armor with None -> "None" | Some x -> x.name

let get_items_names t = t.items

let get_max_items t = t.max_items

let get_melee_weapon t = t.melee

let get_ranged_weapon t = t.ranged

let get_armor_protection t = 
  match t.armor with 
  | Some a -> a.protection
  | None -> 0