(** [init] initializes the game and contains all the actions that need to
    happen before entering the main game loop.*)
val init : unit -> 'a

(** [terminate] contains all the actions that need to happen right before
    exiting the program. This is a safe exit from the game.*)
val terminate : unit -> unit

(** [res_key] contains the logic behind responding to a user key-press [c]*)
val res_key : char -> unit

(** [res_mouse] contains the logic behind responding to a user mouse event 
    at x,y*)
val res_mouse : int -> int -> unit

(** [res_exn] contains the logic behind responding to an exception besides End*)
val res_exn : exn -> unit

(** [game_loop f_init f_end f_key f_mouse f_exn] takes in five functions that 
    represent the different stages of the game and polls for user input. When
    some user input is detected (i.e. mouse press/button down) then the loop
    responds to that event according to f_mouse and f_key respectively. *)
val game_loop : (unit -> 'a) -> (unit -> unit) -> 
  (char -> unit) -> (int -> int -> unit) -> (exn -> unit) -> unit