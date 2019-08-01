module a12

exception HeapError of string

type Heap<'a when 'a: equality> =
  | EmptyHP
  | HP of 'a * Heap<'a> * Heap<'a>

(* Q1.1 *)

let ex3 = HP(1,HP(2,HP(3,EmptyHP,EmptyHP),HP(5,EmptyHP,EmptyHP)), HP(4,EmptyHP,EmptyHP))
let exFalse = HP(5,HP(2,HP(3,EmptyHP,EmptyHP),HP(5,EmptyHP,EmptyHP)), HP(4,EmptyHP,EmptyHP))

// Tyoe : Heap<’a when ’a: equality> is a polymorphic as the heap can take any type as long as there is equality between the values

let empty = EmptyHP

(* Q1.2 *)
let isEmpty h = 
  match h with
  | EmptyHP -> true
  | _ -> false 
  ;;

let rec size h = 
  match h with
  | EmptyHP -> 0
  | HP(root,left,right) -> 1 + size left + size right
  | HP(_,left,_) -> 1 + size left
  | HP(_,_,right) -> 1 + size right
  

let find h = 
  match h with
  | EmptyHP -> raise (HeapError "Empty heap")
  | HP(root,_,_) -> root

let rec chkHeapProperty' h = 
  match h with
  | EmptyHP -> true
  | HP(root,left,right) -> 
            match left with
              | EmptyHP -> true
              | HP(root',_,_) -> if root > root' then false else chkHeapProperty' left 
            match right with
              | EmptyHP -> true
              | HP(root',_,_) -> if root > root' then false else chkHeapProperty' right

let rec chkHeapProperty h = 
  match h with
  | EmptyHP -> true
  | HP(root,left,right) -> 
            match left with
              | EmptyHP -> true
              | HP(root',_,_) -> if root < root' then chkHeapProperty left else false  
            match right with
              | EmptyHP -> true
              | HP(root',_,_) -> if root < root' then chkHeapProperty right else false

chkHeapProperty' ex3
chkHeapProperty' exFalse
chkHeapProperty ex3
chkHeapProperty exFalse


(* Q1.3 *)
let rec map f h = 
  match h with
  | EmptyHP -> EmptyHP
  | HP(root,left,right) -> HP(f root, map f left, map f right) 
   
map (fun x -> x + 1) ex3

let f = function
  | x -> if x % 2 = 0 then x * x else x

map f ex3;;
let heap = HP(3,HP(1,EmptyHP,EmptyHP),EmptyHP)
chkHeapProperty (map f ex3);;

    
(* Divide And Conquer *)
(* Q2.1 *)
let random =
  let rnd = System.Random()
  fun () -> rnd.Next(1,10000)
  
let genRandoms n = 
  Array.init n (fun x -> random()) 


let genRandomsP n =
  Array.Parallel.init n (fun x -> random()) 

(* Q2.2 *)
(* Mergesort *)
let split xs = match xs with
  |[] -> [] 
  | _ -> List.splitInto 2 xs

split [1;3;5;6];;

let indivisible xs = 
  match xs with
  | [] -> true
  | _ when xs.Length = 1 -> true
  | _ -> false



let rec merge (xs,ys) =
  match xs,ys with
  | [],[] -> []
  | x::xs,[] -> x::merge(xs,ys)
  | [],y::ys -> y::merge(xs,ys)
  | x::xs,y::ys ->
    match x,y with
    | x,y when x <= y -> x::merge (xs,y::ys) 
    | x,y when x >= y -> y::merge (x::xs,ys) 

let v1 = [1;4;5];;
let v2 = [3;5;2];;

let v3 = merge (v1,v2);;
merge ([1;3;4;5],[1;2;7;9])    

    
(* Q2.3 *)
let divideAndConquer s m i p = failwith "not implemented"

let divideAndConquer split merge indivisible p =
  let rec dc p = 
    if indivisible pthen ...else ...dc p

(* Q3.1 *)
let triNum = failwith "not implemented"
let triNumC = failwith "not implemented"

(* Q3.2 *)
let rec filterOddIndex s =
  Seq.append (Seq.singleton (Seq.item 0 s))
             (filterOddIndex (Seq.skip 2 s))

let rec myFilterOddIndex s = failwith "not implemented"
 
(* Q3.3 *)
let rec seqZip s1 s2 = failwith "not implemented"

(* Q4.1 *)
exception FigError of string
type Point = P of double * double

type Fig =
    Circle of Point * double 
  | Line of Point * Point
  | Move of double * double * Fig
  | Combine of Fig list
  | Label of string * Fig
  | Ref of string

let rectEx = failwith "not implemented"

let rect (x1,y1) (x2,y2) = failwith "not implemented"

(* Q4.2 *)
let buildEnv fig = failwith "not implemented"

(* Q4.3 *)
let rec substFigRefs env fig = failwith "not implemented"

(* Q4.4 *)
let reduceMove fig = failwith "not implemented"

