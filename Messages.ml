open Graphics

type player_stats = {
  level : int;
  exp : int;
  max_exp : int;
  health : int;
  max_health : int;
  energy : int;
  max_energy : int;
  turns_played : int;
  floor : int;
}

type inventory = {
  melee : string;
  ranged : string;
  head : string;
  torso : string;
  legs : string;
  feet : string;
  items : string list;
  max_items : int;
}

type msgs = string list

(** The strings providing helpful information on game controls. *)
let help_strings = 
  ["INSTRUCTIONS/CONTROLS:"; 
   "Press i,j,k,l to move up, left, down, right"; 
   "Press b to break the 4 walls near you. "^ 
   "Walk into the staircase ('#') to advance to the next level"; 
   "Press [spacebar] to rest; Press [e] to see inventory"; 
   "Press h to see instructions again."]


let write_msg msg messages =
  match messages with
  | a :: b :: c :: d :: t -> msg :: a :: b :: c :: d :: []
  | lst -> msg :: lst

(** [update_stats stats col] updates the stats of the player to [stats]. *)
let update_stats stats col = 
  Graphics.set_color col;
  Graphics.moveto 8 700;
  Graphics.draw_string ("Level: " ^ (string_of_int stats.level));
  Graphics.moveto 8 688;
  Graphics.draw_string ("Exp: " ^ (string_of_int stats.exp) ^ "/" ^ 
                        (string_of_int stats.max_exp));
  Graphics.moveto 8 676;
  Graphics.draw_string ("Health: " ^ (string_of_int stats.health) ^ "/" ^ 
                        (string_of_int stats.max_health));
  Graphics.moveto 8 664;
  Graphics.draw_string ("Energy: " ^ (string_of_int stats.energy) ^ "/" ^ 
                        (string_of_int stats.max_energy));
  Graphics.moveto 150 700;
  Graphics.draw_string ("Turns Lived: " ^ (string_of_int stats.turns_played));

  Graphics.moveto 150 688;
  Graphics.draw_string ("Difficulty: " ^ (string_of_int stats.floor))

(** [show_messages msgs col] shows the messages in the specified color [col]. *)
let show_messages msgs col =
  Graphics.set_color col;
  let printer x acc =
    Graphics.moveto 278 acc;
    Graphics.draw_string x;
    acc - 12 in
  List.fold_right printer msgs 700

(** [draw_divider col] draws the divider in the color [col]. *)
let draw_divider col = 
  Graphics.set_color col;
  Graphics.fill_rect 0 641 3 79;
  Graphics.fill_rect 0 641 1280 3;
  Graphics.fill_rect 1277 641 3 79;
  Graphics.fill_rect 0 716 1280 3;
  Graphics.fill_rect 270 641 3 79

let write_msgs new_msgs old_msgs = 
  let rec helper new_messages old_messages =
    match new_messages with
    | [] -> old_messages;
    | h::t -> helper t (write_msg h old_messages)
  in helper new_msgs old_msgs

let write_help msgs = write_msgs help_strings msgs

let print_lst lst = 
  List.fold_left (fun acc x -> acc ^ "; " ^ x) "" lst

let write_inventory inventory msgs = 
  let inv_strings = [
    "Melee: " ^ inventory.melee;
    "Ranged: " ^ inventory.ranged;
    "Head: " ^ inventory.head ^ "  Torso: " ^ inventory.torso ^ 
    "  Legs: " ^ inventory.legs ^ "  Feet: " ^ inventory.feet;
    "Items: " ^ print_lst inventory.items;
    "";
  ] in
  write_msgs inv_strings msgs

let draw_ui p_stats msgs stats_col msg_col div_col = 
  draw_divider div_col;
  update_stats p_stats stats_col;
  ignore(show_messages msgs msg_col)