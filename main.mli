open Graphics

type state

exception End

(** [draw_point x y width height color] fills all the character-sized 
    units (defined in state) of the screen with a certain Graphics.color*)
val draw_point : int -> int -> int -> int -> color -> unit

(** [draw_char x y char_width char_height color character] draws a character
    [c] at [x],[y] that is the size of [char_width]x[char_height] in pixels*)
val draw_char : int -> int -> int -> int -> color -> char -> unit

(** [clear_area x1 y1 x2 y2 color] clears the area defined by the corners
    [x1] [y1] [x2] [y2]*)
val clear_area : int -> int -> int -> int -> color -> unit

(** [init s] initializes the game and contains all the actions that need to
    happen before entering the main game loop.*)
val init_game : state -> unit

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