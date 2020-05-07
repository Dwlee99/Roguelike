(** Manages all of the text displayed to the player. Any and all messages,
    updates, health bar, food, etc*)

open Graphics

(** A type representing the player data that the system needs to print the
    player's information. *)
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

(** A type representing the player's inventory that the system needs to print
    thee player's inventory out*)
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

(** The list of recent messages. *)
type msgs = string list

(** 
   MSG are messages that are meant to display some information about the
   actions of the player. Ex. 'You walk into a wall. It seems sturdy.' 

   STATS are messages that are meant to display the player's statistics like
   health, food, level, experience, etc.
*)

(** [write_msgs new_msgs old_msgs] is the new list of messages with the messages
    in [new_messages] written to the message list [old_msgs]. *)
val write_msgs : string list -> msgs -> msgs

(** [write_help msgs] is the new list of messages with the help messages 
    written to the current list [msgs]. *)
val write_help : msgs -> msgs

(** [write_inventory] writes the player's inventory into the messages panel
    showing the items they carry and the equipment they are wearing*)
val write_inventory : inventory -> msgs -> msgs

(** Draws the entire top area in the game including the dividers, player stats
    and messages*)
val draw_ui : player_stats -> msgs -> Graphics.color -> 
  Graphics.color -> Graphics.color -> unit


