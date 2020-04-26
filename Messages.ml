open Graphics

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

let msg_x = 400
let msg_y = 700

let write_msg msg = function
  | a :: b :: c :: d :: t -> msg :: a :: b :: c :: d :: []
  | lst -> msg :: lst

let update_stats player col = 
  Graphics.set_color col;
  Graphics.moveto 8 700;
  Graphics.draw_string ("Level: " ^ (string_of_int player.level));
  Graphics.moveto 8 688;
  Graphics.draw_string ("Exp: " ^ (string_of_int player.exp) ^ "/" ^ 
                        (string_of_int player.max_exp));
  Graphics.moveto 8 676;
  Graphics.draw_string ("Health: " ^ (string_of_int player.health) ^ "/" ^ 
                        (string_of_int player.max_health));
  Graphics.moveto 8 664;
  Graphics.draw_string ("Energy: " ^ (string_of_int player.energy) ^ "/" ^ 
                        (string_of_int player.max_energy));
  Graphics.moveto 150 700;
  Graphics.draw_string ("Turns Lived: " ^ (string_of_int player.turns_played))

let show_messages lst col =
  Graphics.set_color col;
  let printer x acc =
    Graphics.moveto 278 acc;
    Graphics.draw_string x;
    acc - 12 in
  List.fold_right printer lst 700

let draw_divider col = 
  Graphics.set_color col;
  Graphics.fill_rect 0 641 3 79;
  Graphics.fill_rect 0 641 1280 3;
  Graphics.fill_rect 1277 641 3 79;
  Graphics.fill_rect 0 716 1280 3;
  Graphics.fill_rect 270 641 3 79

let draw_ui player msgs text_col div_col = 
  draw_divider div_col;
  update_stats player text_col;
  ignore(show_messages msgs text_col)