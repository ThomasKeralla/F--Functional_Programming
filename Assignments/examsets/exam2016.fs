open System.Security.AccessControl
// --------- Question 1  -------------

type Multiset<'a when 'a: comparison> = MSet of Map<'a, int>

let ex = MSet (Map.ofList [("a",1);("b",2);("c",1)])
let ex2 = MSet (Map.ofList [("c",1);("a",2);("z",1)])

let diceSet = MSet (Map.ofList [(1,2);(2,1);(3,5);(4,0);(5,2);(6,2)])
//diceSet has the type Multiset<int>

// no this can not be done as the Multiset with functions are not comparable 

// Question 1.2

let newMultiset = MSet Map.empty

let isEmpty (MSet ms) = 
    Map.isEmpty ms
    
let add k (MSet ms) = 
    if Map.containsKey k ms then (Map.add k ((Map.find k ms)+1) ms) |> MSet else (Map.add k 1 ms) |> MSet 

let del k (MSet ms) = 
    if Map.containsKey k ms then 
        let r = Map.find k ms
        if r > 1 then (Map.add k (r - 1)) ms |> MSet else (Map.remove k ms) |> MSet
    else  ms |> MSet

//let fr = add "a" ex;;
//let fr2 = del "a" fr;;

let toList (MSet ms) = List.map (fun (x,y) -> x ) (Map.toList ms)

let fromList l = 
    let rec inner l ms = 
        match l with
        | [] -> ms
        | x::xs -> inner xs (add x ms)
    inner l newMultiset

let map f (MSet ms) =
  Map.toList ms |> List.map (fun (x ,y) -> (f x), y) |> Map.ofList |> MSet  


let fold f a (MSet ms) =
  Map.toList ms |> List.map (fun (x,y) -> x) |> List.fold f a

let union (MSet ms1) ms2 = 
  let rec inner ms1' ms2' =
    match ms1' with
    | [] -> ms2' 
    | (x,n1)::ms1' -> if n1 > 1 then inner ((x,(n1 - 1))::ms1') (add x ms2') else inner ms1' (add x ms2')
  inner (Map.toList ms1) ms2


//wrong
//let union (MSet ms1) (MSet ms2) = 
  //Map.toList ms1 @ Map.toList ms2 |> List.fold (fun acc (x,y) -> add x acc) newMultiset

(*
let rec minus ms1 ms2 =
    match Map.toList ms1 with 
    | [] -> ms2 |> MSet
    | (x,n)::ms1 -> 
      match Map.tryFind x ms2 with
      | Some i -> if n > i then minus (Map.ofList ms1) (Map.remove x ms2) else minus (Map.ofList ms1)(Map.add x (i - n) ms2 )
      | None -> minus ms1 ms2
      *)

let rec minus (MSet ms1) (MSet ms2) =
    let m' = Map.toList ms1
    match m' with 
    | [] -> ms2 |> MSet
    | (x,n)::m' -> 
      match Map.tryFind x ms2 with
      | Some i -> if n >= i then minus ((Map.ofList m')|> MSet) ((Map.remove x ms2)|> MSet) else minus ((Map.ofList m')|> MSet) ((Map.add x (i - n) ms2 )|> MSet)
      | None -> minus (Map.ofList m' |> MSet) (ms2 |> MSet)

// ------ Question 2 ------

// the functions prints n + 1 chars, where n is the gap between a integer < 10 and 10, always starting  
// with the char defined by the function called. If n is even an odd number of chars is printed
// and odd gives a equal number eg. f 8 = fgf f 9 = fg 

// g 4

// no 

let rec f n acc =
    if n < 10 then g (n+1) (acc+"f") else (acc+"f")
and g n acc =
    if n < 10 then f (n+1) (acc+"g") else (acc+"g")

// a sequence of 0 to m repeated n many times

// no
let myFinSeq2 (n,m) = seq { 
    for i in [0 .. n] do
        yield (i, seq { 
            for j in [0 .. m] do 
                yield j })}


// ----- Questions 4 -----

type Row = int
  type Col = char
  type CellAddr = Row * Col
  type ArithOp = Add | Sub | Mul | Div
  type RangeOp = Sum | Count
  type CellDef =
      FCst of float
    | SCst of string
    | Ref of CellAddr
    | RangeOp of CellAddr * CellAddr * RangeOp
    | ArithOp of CellDef * ArithOp * CellDef
  type CellValue =
      S of string
    | F of float
  type Sheet = Map<CellAddr,CellDef>


 



  