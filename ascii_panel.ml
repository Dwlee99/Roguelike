open Graphics

type game_unit = {
  char_width : int;
  char_height : int;
}

type t = {
  screen_width : int;
  screen_height : int;
  background_color : Graphics.color;
  sizes : game_unit;
}

let board_font = "consolas"

let open_window w h bcolor = 
  open_graph (" " ^ (string_of_int w) ^ "x" ^ (string_of_int h));
  set_window_title "CS 3110 Project : Roguelike";
  {
    screen_width = w;
    screen_height = h;
    background_color = bcolor;
    sizes = {
      char_width = w / 80;
      char_height = w / 80;
    }
  }

let draw_point x y c t =
  set_color c;
  fill_rect (t.sizes.char_width * x) (t.sizes.char_height * y) 
    t.sizes.char_width t.sizes.char_height;
  t

let draw_char x y c chr t = 
  set_color c;
  (*set_font board_font;*)
  moveto (t.sizes.char_width * x) (t.sizes.char_height * y);
  draw_char chr;
  t

let fill_rect x y w h c t =
  set_color c;
  fill_rect (t.sizes.char_width * x) (t.sizes.char_height * y) 
    (t.sizes.char_width * w) (t.sizes.char_width * h);
  t

let clear_graph t =
  fill_rect 0 0 t.screen_width t.screen_height t.background_color t