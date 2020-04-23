open Graphics

type state

exception End

(** [init s] initializes the game and contains all the actions that need to
    happen before entering the main game loop and returns panel information. *)
val init_game : state -> Ascii_panel.t

(** [terminate s] contains all the actions that need to happen right before
    exiting the program. This is a safe exit from the game.*)
val stop_game : state -> unit

(** [res_key s] contains the logic behind responding to a user key-press [c]*)
val res_key : char -> state -> unit

(** [res_exn s] contains the logic behind responding to an exception 
    besides End*)
val res_exn : exn -> state -> unit

(** [game_loop f_init f_end f_key f_mouse f_exn] takes in five functions that 
    represent the different stages of the game and polls for user input. When
    some user input is detected (i.e. mouse press/button down) then the loop
    responds to that event according to f_mouse and f_key respectively. *)
val game_loop : (state -> unit) -> 
  (state -> unit) -> 
  (char -> state -> unit) ->  
  (exn -> state -> unit) ->
  unit