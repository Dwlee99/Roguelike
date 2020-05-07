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
  | Inv
  | Melee_Attack of direction
  | Ranged_Attack of direction


let parse = function
  | 'i' -> Move Up
  | 'j' -> Move Left
  | 'k' -> Move Down
  | 'l' -> Move Right
  | 'b' -> Break
  | 'h' -> Help
  | 'e' -> Inv
  | _ -> Rest