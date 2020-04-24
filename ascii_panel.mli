open Graphics

(** Controls the window opened and controlled by the Graphics library. *) 

(** The units by which characters will be drawn on the panel. *)
type game_unit = {
  char_width : int;
  char_height : int;
  font_size : int;
}

(** Holds information imporant to the ASCII Panel *)
type t

(** [open_window width height] opens the window with [width]x[height]
    resolution and is the default state for the panel. *)
val open_window : int -> int -> t

(** [draw_point x y width height color t] fills in the game coordinate 
    ([x], [y]) of the screen with [color]. Note: [x] and [y] are not pixels, but 
    [game_unit]s. *)
val draw_point : int -> int -> color -> t -> t

(** [draw_char x y color character t] draws the character [c] at ([x], [y]) with
    [t]'s [game_unit] sizes using [color]. *)
val draw_char : int -> int -> color -> char -> t -> t

(** [draw_rect x y w h color t] draws a rectangle at ([x],[y]) with
    width [w], height [h] and [color]. All numerical inputs are measured in
    [game_unit]s, not pixels.*)
val fill_rect : int -> int -> int -> int -> color -> t -> t

(** [clear_graph t] clears the screen of all things drawn. *)
val clear_graph : t -> t