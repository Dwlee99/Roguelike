type protection = int

type armor_type = 
  | Head
  | Torso
  | Legs
  | Feet

type armor = {
  name : string;
  a_type : armor_type;
  protection: protection;
}