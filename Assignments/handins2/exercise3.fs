open System
module e3

(*
    Exercise 3.1Write a function downTo:int->int listso that downTo n returns the n-element list[n; n-1;...; 1].   
    You must use if-then-else expressions to define the function.
    Secondly define the functiondownTo2having same semantics asdownTo. This time you must use patternmatching.
*)

let rec downTo x = 
    if x < 0 then failwith "negative input" elif x > 0 then x::downTo (x-1) else []  
let res1 = downTo 10;;

let rec downToPattern x = 
    match x with
    | 0 -> []
    | x -> x::downToPattern (x-1)
    | _ -> failwith "negative input" 
let res2 = downToPattern 10;;

(*
    xercise 3.2 Write a function removeOddIdx:int list->int list so that removeOddIdx xs removes the odd-indexed 
    elements from the listxs
*)
let rec removeOddIdx xs =
    match xs with 
    | [] -> []
    | x::xs when x % 2 = 0 -> x::removeOddIdx xs
    | x::xs -> removeOddIdx xs
let res2 = removeOddIdx [1;2;3;4;5;6;7];;

(*
    xercise 3.3Write a function combinePair:int list->(int*int) listso that combinePair xs returns the 
    list with elements fromxscombined into pairs.  If xs contains an odd number of elements, then the 
    last element is thrown away:
*)
let rec combinePair xs =
    match xs with 
    | [] -> []
    | x::[] -> []
    | x::y::xs -> (x,y)::combinePair xs
let res3 = combinePair [1;2;3;4;5;6;7];; 

(*
The former British currency had 12 pence to a shilling and 20 shillings to a pound. Declare
functions to add and subtract two amounts, represented by triples (pounds, shillings, pence) of
integers, and declare the functions when a representation by records is used. Declare the func-
tions in infix notation with proper precedences, and use patterns to obtain readable declarations.
*)

let addPoundx x y = 
    match x,y with
    | (a,b,c), (a',b',c') -> (a+a'+(b+b'+(c+c'/12))/20,(b+b'+((c+c')/12))%20,(c+c')%12)
//recursive version 
let rec addPoundsRec x y = 
    match x,y with
    | (a,b,c), (a',0,0) -> (a+a',b,c)
    | (a,b,c), (a',b',0) -> addPoundsRec (a+(b+b')/20, (b+b')%20,c) (a',0,0)
    | (a,b,c), (a',b',c') -> addPoundsRec (a,b+(c+c')/12,(c+c')%12) (a',b',0)

let rec subtractPounds x y = 
    match x,y with
    | (a,b,c), (0,0,0) -> (a+(-((b+ c/12)/20)),((b+ c/12)%20),(c%12))
    | (a,b,c), (a',0,0) -> if a'> a then subtractPounds (a-(a'-1),b+(-19),c+(-12)) (0,0,0)
                                    else (a-a',b,c)
    | (a,b,c), (a',b',0) -> if b' > b then subtractPounds (a-1,(b+20)-b',c) (a',0,0) 
                                      else subtractPounds (a,b-b',c) (a',0,0)
    | (a,b,c), (a',b',c') -> if c' > c then subtractPounds (a,b-1,(c+12)-c') (a',b',0) 
                                        else subtractPounds (a,b,c-c') (a',b',0)
let resSubtract = subtractPounds (2,2,2) (1,1,1);;
let resSubtract2 = subtractPounds (1,1,1) (2,2,2);;
let resSubtract3 = subtractPounds (0,0,0) (1,1,1);;
// 3.3

let (.+.) x y = 
    match x,y with
    | (a,b),(c,d) -> (a+c,b+d)

let (.*.) x y =
    match x,y with 
    | (a,b),(c,d) -> (a*c-b*d,b*c + a*d)


let (.-.) x y =
    match x,y with
    | (a,b), (c,d) -> (a,b) .+. (-c,-d)

let (./.) x y = 
    match x,y with
    | (0,0),(c,d) -> failwith "a and b cannot both be 0"
    | (a,b),(c,d) ->  let div = a*a+b*b 
                      (c*a/div,d*(-b)/div) 
// do not think above is correct, please correct me if my math is of