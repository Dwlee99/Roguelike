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
  | Display_Melee
  | Display_Ranged
  | Melee_Attack of direction
  | Ranged_Attack of direction
  | None

let parse = function
  | 'i' -> Move Up
  | 'j' -> Move Left
  | 'k' -> Move Down
  | 'l' -> Move Right
  | 'b' -> Break
  | 'h' -> Help
  | 'a' -> Display_Melee
  | 'r' -> Display_Ranged
  | 'e' -> Inv
  | _ -> Rest

let parse_two c1 c2 =
  match c1, parse c2 with
  | 'a', Move dir -> Melee_Attack dir
  | 'r', Move dir -> Ranged_Attack dir
  | _ -> None