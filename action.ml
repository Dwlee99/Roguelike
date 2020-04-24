type t = 
  | Left
  | Right
  | Up
  | Down
  | Rest

let parse = function
  | 'i' -> Up
  | 'j' -> Left
  | 'k' -> Down
  | 'l' -> Right
  | _ -> Rest