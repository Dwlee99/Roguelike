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

(** Draws the entire top area in the game including the dividers, player stats
    and messages*)
val draw_ui : player_stats -> msgs -> Graphics.color -> Graphics.color 
  -> unit
