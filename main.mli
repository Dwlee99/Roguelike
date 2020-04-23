open Graphics

type state

exception End

(* [draw_point] draws a rectangle at [x*xs], [y*ys] where xs and ys 
   represent the scale of our unit of screen in the [x] and [y] direction 
   respectively using color [c]*)
val draw_point : int -> int -> int -> int -> color -> unit

val draw_char : int -> int -> int -> int -> color -> char -> unit

val clear_area : int -> int -> int -> int -> color -> unit

(** [init] initializes the game and contains all the actions that need to
    happen before entering the main game loop.*)
val init_game : state -> unit

(** [terminate] contains all the actions that need to happen right before
    exiting the program. This is a safe exit from the game.*)
val stop_game : state -> unit

(** [res_key] contains the logic behind responding to a user key-press [c]*)
val res_key : char -> state -> unit

(** [res_mouse] contains the logic behind responding to a user mouse event 
    at x,y*)
val res_mouse : int -> int -> state -> unit

(** [res_exn] contains the logic behind responding to an exception besides End*)
val res_exn : exn -> state -> unit

(** [game_loop f_init f_end f_key f_mouse f_exn] takes in five functions that 
    represent the different stages of the game and polls for user input. When
    some user input is detected (i.e. mouse press/button down) then the loop
    responds to that event according to f_mouse and f_key respectively. *)
val game_loop : (state -> unit) -> 
  (state -> unit) -> 
  (char -> state -> unit) -> 
  (int -> int -> state -> unit) -> 
  (exn -> state -> unit) ->
  unit