(** Manages all of the text displayed to the player. Any and all messages,
    updates, health bar, food, etc*)

open Graphics
open State

(** 
   MSG are messages that are meant to display some information about the
   actions of the player. Ex. 'You walk into a wall. It seems sturdy.' 

   STATS are messages that are meant to display the player's statistics like
   health, food, level, experience, etc.
*)

(** writes a message [string] to the terminal and it displays it to the player
    near the top of the screen*)
val write_msg : string -> Graphics.color -> unit

(** Takes in the current state of the game and updates the player's stats*)
val update_stats : State.t -> Graphics.color -> unit

val draw_ui : State.t -> Graphics.color -> Graphics.color -> Graphics.color -> unit
