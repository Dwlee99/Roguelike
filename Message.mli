(** Manages all of the text displayed to the player. Any and all messages,
    updates, health bar, food, etc*)

open Graphics

(** 
   MSG are messages that are meant to display some information about the
   actions of the player. Ex. 'You walk into a wall. It seems sturdy.' 

   STATS are messages that are meant to display the player's statistics like
   health, food, level, experience, etc.
*)
type msg_type = MSG | STATS

val write_msg : string -> unit

val update_stat : -> unit
