
type PrioritySet<'a when 'a: equality> = 
    | EmptySet
    | PrioritySet of List<'a>


// Question 1.1

// result of inseritng into PrioritySet
let psEx = PrioritySet ["a";"q"; "b";"d"]
let psEx2 = PrioritySet ["a";"z"; "t";"m"]
// has type : int * string list


// Question 1.2
let isEmpty s = 
    match s with
    | EmptySet -> true
    | _ -> false

let rec size p acc =
    match p with
    | EmptySet -> acc
    | (PrioritySet p) -> 
         match (p) with
         | [] -> acc
         | x::p -> size (PrioritySet p) acc+1

let rec contains e ps =
    match ps with 
    | EmptySet -> false
    | (PrioritySet p) -> 
        match p with
        | [] -> false
        | x::p -> if x = e then true else contains e (PrioritySet p)


let getPN e ps =
        match ps with 
        | EmptySet ->  failwith "empty set"
        | (PrioritySet p) -> 
            let rec inner n e p =
                match p with
                | [] -> failwith "value not in this PrioritySet"
                | x::p -> if x = e then n+1 else inner (n + 1) e p
            inner 0 e p

// Question 1.3

let remove e ps =
    match ps with
    | EmptySet ->  failwith "empty set"
    | (PrioritySet p) -> 
        let rec inner e p acc =   
            match p with
            | [] -> acc
            | x::p -> if x = e then acc @ p  else inner e p (acc @ [x])
        inner e p []

remove "b" psEx;;

let add e ps =
    match ps with
    | EmptySet ->  failwith "empty set"
    | (PrioritySet p) -> p @ [e]


let map f ps =
    match ps with 
    | EmptySet ->  failwith "empty set"
    | (PrioritySet p) ->
        let rec inner f p acc =
            match p with
            | [] -> acc
            | x::p -> inner f p ((f x)::acc)
        inner f p []

let cp ps1 ps2 =
    match ps1,ps2 with
    | EmptySet,_ -> EmptySet
    | _,EmptySet -> EmptySet
    | (PrioritySet p1),(PrioritySet p2) -> List.allPairs p1 p2 |> PrioritySet 


// Question 2 

let f curRow =
    let rec f' = function
        |[] -> []
        |[_] -> [1]
        | xs -> let (x1::x2::xs) = xs 
                x1 + x2 :: f' (x2::xs)
    (1 :: f' curRow)

// above function computes the sum of every index plus the next index with a 1 at index 0
// and a 1 at index n-1

let fMatch curRow =
    let rec fMatch' = function
        |[] -> []
        | xs when xs.Length = 1 -> [1]
        | xs -> let h = List.head xs
                let z =  xs.[1]   
                h + z :: fMatch' (List.tail xs)
    (1 :: fMatch' curRow)

// The warning disappears because with the cons operation there is a implicit pattern match were all cases are not covered
// using the f# library there is no implicit pattern match anymore thus the warning disappears 

let fMatch2 curRow =
    let rec fMatch' l acc = 
        match l with
        |[] -> acc
        | xs when xs.Length = 1 -> 1::acc
        | xs -> let h = List.head xs + xs.[1]   
                fMatch' (List.tail xs) (h::acc)
    (fMatch' curRow [1])

// Question 3

// All permutations are generated in the order based on the indexes of s1 
// s1.[0] to all indexes of s2, s1[1] to all indexes of s2 etc.

// no, as the return has to be a sequence of list a' with the permutation  s1[index],s2[index]

let mySeq2 s1 s2 =
    seq { for e1 in s1 do
            for e2 in s2 do
              yield (e1,e2) }

mySeq2 [1;2] ['A';'B';'C'];;

// Question 3.3

let mySeq3 n = Seq.initInfinite (fun x -> n*n - n*x) 

// Question 4 

type DataSpec =
  | RangeInt of int * int
  | ChoiceString of string list
  | StringSeq of string
  | Pair of DataSpec * DataSpec
  | Repeat of int * DataSpec
  | Pick of string
  | Label of string * DataSpec

let reg =
    Repeat(3,Pair(StringSeq "a",
                  Pair(ChoiceString["cheese";"herring";"soft drink"],
                       RangeInt(1,100))))


let pur = Repeat(2,Pair(RangeInt(1,10),StringSeq("a")))
   


let genValue ds = 
    let inner ds acc =
        match ds with 
         | RangeInt(x,y) -> acc + "(%s)" next (x,y)  
         | ChoiceString of string list 
         | StringSeq of string
         | Pair of DataSpec * DataSpec
         | Repeat of int * DataSpec
    "["+( inner ds "") +"]" 

     "[(a1,(cheese,69));(a2,(herring,94));(a3,(cheese,50))]"

(* 4.2 *)

let rand = System.Random()
let next(i1,i2) = rand.Next(i1,i2)

let numGen = let n = ref 0
             fun () -> n := !n+1; !n

let rec genValue ds =
    let rec genValue' ds' =
        match ds' with
        | RangeInt(i1,i2) -> next(i1,i2).ToString()
        | StringSeq str -> str + (string (numGen ()))
        | ChoiceString strList -> List.item (next(1,strList.Length)) strList
        | Pair(ds1,ds2) -> "(" + (genValue' ds1) + "," + (genValue' ds2) + ")"
        | Repeat(n,ds'') -> if n = 0 then "" else (genValue' ds'') + ";" + (genValue' (Repeat((n-1),ds'')))
    "[" + (genValue' ds) + "]"

genValue pur
genValue reg

(* 4.3 *)

type Env = Map<string,string list>

let addToEnv s v dEnv =
    if Map.containsKey s dEnv then Map.add s (v::(Map.find s dEnv)) dEnv else Map.add s [v] dEnv

let e = Env [ ("1", ["one"]); ("2", ["two";"three";"ten";"eight"]) ]
 
addToEnv "1" "9" e;; 

let pickFromEnv s dEnv =
        if Map.containsKey s dEnv then (Map.find s (dEnv:Env)).[next(0,(Map.find s (dEnv:Env)).Length)]
         else failwith "not happening"

pickFromEnv "2" e;;


