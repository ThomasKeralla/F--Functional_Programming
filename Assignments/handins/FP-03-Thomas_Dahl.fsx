//Assignment 3
module assignment3
open System;;

//3.1
let rec downTo n = 
    if n < 1 then raise (System.ArgumentException("Input can not be negative"))
    else if n = 1 then [1]
    else n :: downTo (n-1)
;;

let rec downTo2 n = 
    match n with
    |n when n < 1 -> raise (System.ArgumentException("Input has to be min. 1"))
    |1 -> [1]
    |_ -> n:: downTo2 (n-1)
 ;;

//3.2
 let rec removeOddIdx list = 
    match list with 
    | h::list when h % 2 <> 0 -> removeOddIdx list
    | h::list -> h :: (removeOddIdx list)
    | []    -> []
;;
removeOddIdx [-1;-4;2;3;5;7;8;134;6];;

//3.3
let rec combinePair list =
    match list with
    | x::x2::list -> (x,x2):: combinePair list
    | x::list -> []
    | []-> []
 ;;
    
;;
combinePair [2;4;6;4];;

//3.4

let pence (x,y,z) = 
    z+y*12+x*240
pence (-2,-2,-2);;

let poundify n = 
    (n/240,(n%240)/12,(n%240)%12)

let add x y = 
  poundify (pence x + pence y) 

let subtract x y =
    poundify (pence x - pence y) 

(*
//goofing around
//calculate pounds
let rec cal (x,y,z) =
    match x,y,z with
    |(_,_,z) when z / 12 >= 1 -> cal (x,y+z/12,z%12)
    |(_,y,_) when y / 20 >= 1 -> (x+y/20,y%20,z)
    |_ -> (x,y,z)
    ;;
cal (1,1,12);;

//using pattern matching to convert to shilling
let rec pence2 (x,y,z) =
    match x,y,z with
    |(x,_,_) when x <> 0 -> pence2 (x-x,y+x*20,z)
    |(_,y,_) when y <> 0 -> pence2 (x,y-y,z+y*12)
    |_ -> z
;;
pence2 (0,0,0);;

//with inner functions
let addP x y =
    let poundify = function
        x   -> (x/240, x%240/12, x%240%12)
    let pencify = function
        (p,s,pp) -> p*240 + s * 12 + pp
    let px = pencify x
    let py = pencify y
    poundify (px + py)       
*)
//3.5

//3.5.1
let (.+) x y  = 
    match x, y with 
        | (a,b),(c,d) -> (a+c,b+d)

exception InnerError of string
let (.*) x y  = 
    match x, y with 
        | (a,b),(c,d) -> (a*c,b*d)
        |_ -> raise (InnerError "wrong input")

(4,5) .* (2,2);;     

//3.5.2
let (.-) x y = 
    match x, y with
        | (a,b), (c,d) -> (a,b) .+ (-c,-d)
(4,4) .- (3,3);;

let (./) x y =
    match x, y with
        |(0,b), (c,d) -> raise (System.ArgumentException("Input can not be 0"))
        |(a,0), (c,d) -> raise (System.ArgumentException("Input can not be 0"))
        |(a,b), (c,d) ->  (c*a/(a*a+b*b),d*(-b)/(a*a+b*b)) 
       

(2,2) ./ (4,4);;

//3.5.3 
let (../) x y =
    match x, y with
        |(0,b), (c,d) -> raise (System.ArgumentException("Input can not be 0"))
        |(a,0), (c,d) -> raise (System.ArgumentException("Input can not be 0"))
        |(a,b), (c,d) ->  let ss = a*a+b*b 
                          (c*a/ss,d*(-b)/ss) 
       

(2,2) ../ (4,4);;




//3.6
let rec altsum = function
| [] -> 0 
| x0::xs -> x0 - altsum xs
;;

altsum [1;2;3;4;5;6;7;8];;
altsum [1;2;3];;

