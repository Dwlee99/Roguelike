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

