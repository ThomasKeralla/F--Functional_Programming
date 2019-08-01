
// Questions 1

type item = {id : int;
             name : string;
             price : float}
type register = item list

let i = {id=1;name="milk";price=8.75}
let reg =  [{id=1;name="milk";price=8.75};
                    {id=2;name="Juice";price = 16.25};
                    {id=3;name="Rye Bread";price = 25.00};
                    {id=4;name="White Bread";price=18.50}]


exception Register of string 


let rec getItemById r i =
    match r with
    | [] -> raise (Register "no such id")
    | {id=x;name=y;price=z}::r -> if x = i then {id=x;name=y;price=z} else getItemById r i 

let rec nextId = function
  [] -> 1
| {id=id;name=_;price=_} :: rs -> let maxId = nextId rs
                                  if maxId < id then id+1 else maxId

let addItem s p r = List.rev ({id=(nextId r); name=s; price = p} :: List.rev r)

let rec deleteItemById i r =
    match r with
    | [] -> []
    | ({id=x;name=y;price=z} as item)::r -> let rest = deleteItemById i r  
                                            if x = i then rest else item::rest

let uniqueRegister01 r = let ids = List.map (fun {id=id;name=_;price=_} -> id) r
                         List.length ids = List.length (Set.toList (Set.ofList ids)) 

let itemsInPriceRange f1 f2 r =
    let rec inner f1' f2' r' acc =
        match r' with
        | [] -> acc
        | ({id=x;name=y;price=z} as item)::r' -> if (f1' < z && z < f2') then inner f1' f2' r' (item::acc) else inner f1' f2' r' acc
    inner f1 f2 r []

