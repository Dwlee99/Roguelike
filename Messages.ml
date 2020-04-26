open Graphics
open State

let msg_list = []

let msg_x = 400
let msg_y = 700

let write_msg msg col =
  Graphics.set_color col;
  Graphics.moveto msg_x msg_y;
  Graphics.draw_string msg

let update_stats player col = 
  Graphics.set_color col;
  Graphics.moveto 8 700;
  Graphics.draw_string ("Level: " ^ (string_of_int player.level));
  Graphics.moveto 8 688;
  Graphics.draw_string ("Exp: " ^ (string_of_int player.exp) ^ "/" ^ (string_of_int player.max_exp));
  Graphics.moveto 8 676;
  Graphics.draw_string ("Health: " ^ (string_of_int player.health) ^ "/" ^ (string_of_int player.max_health));
  Graphics.moveto 8 664;
  Graphics.draw_string ("Energy: " ^ (string_of_int player.energy) ^ "/" ^ (string_of_int player.max_energy));
  Graphics.moveto 150 700;
  Graphics.draw_string ("Turns Lived: " ^ (string_of_int player.turns_played))

let draw_divider col = 
  Graphics.set_color col;
  Graphics.fill_rect 0 641 3 79;
  Graphics.fill_rect 0 641 1280 3;
  Graphics.fill_rect 1277 641 3 79;
  Graphics.fill_rect 0 716 1280 3;
  Graphics.fill_rect 270 641 3 79

let draw_ui player msg_col stat_col div_col = 
  draw_divider div_col;
  update_stats player stat_col