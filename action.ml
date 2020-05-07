type direction = 
  | Up
  | Down
  | Left
  | Right

type t = 
  | Move of direction
  | Break
  | Rest
  | Help
  | Attack of direction


let parse = function
  | 'i' -> Move Up
  | 'j' -> Move Left
  | 'k' -> Move Down
  | 'l' -> Move Right
  | 'b' -> Break
  | 'h' -> Help
  | _ -> Rest