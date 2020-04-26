type direction = 
  | Up
  | Down
  | Left
  | Right

type t = 
  | Move of direction
  | Break
  | Rest


let parse = function
  | 'i' -> Move Up
  | 'j' -> Move Left
  | 'k' -> Move Down
  | 'l' -> Move Right
  | 'b' -> Break
  | _ -> Rest