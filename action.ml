type direction = 
  | Up
  | Down
  | Left
  | Right

type t = 
  | Modify of modifier
  | Display of displayer
  | Quit
  | None
and modifier = 
  | Move of direction
  | Break
  | Rest
  | Melee_Attack of direction
  | Ranged_Attack of direction
and
  displayer = 
  | Help
  | PlayerHelp
  | FightingHelp
  | Inv
  | Melee
  | Ranged

let parse = function
  | 'i' -> Modify (Move Up)
  | 'j' -> Modify (Move Left)
  | 'k' -> Modify (Move Down)
  | 'l' -> Modify (Move Right)
  | 'b' -> Modify Break
  | 'h' -> Display Help
  | 'a' -> Display Melee
  | 'r' -> Display Ranged
  | 'e' -> Display Inv
  | 'q' -> Quit
  | 'p' -> Display PlayerHelp
  | 'f' -> Display FightingHelp
  | _ -> Modify Rest

let parse_two c1 c2 =
  match c1, parse c2 with
  | 'a', Modify Move dir -> Modify (Melee_Attack dir)
  | 'r', Modify Move dir -> Modify (Ranged_Attack dir)
  | _ -> None