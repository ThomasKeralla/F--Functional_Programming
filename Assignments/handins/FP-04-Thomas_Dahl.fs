
//Assignment 4
module assignment4
open System;;


(* Exercise 4.1 *)

/// Non-recursive
let explode (s:string) = s.ToCharArray() |> List.ofArray
explode "String";;


/// Recursive
let rec explode2 s = 
    match s with
    |"" -> []
    |s -> s.[0] :: explode2 s.[1 .. s.Length-1]
explode2 "String";;


(* Exercise 4.2 *)

/// Regular implosion

let rec implode (cs:List<char>) = 
    match cs with
    |[] -> ""
    |head::cs -> head.ToString() + (implode cs)

implode ['a';'b';'c']  ;;

let implode2 (cs:List<char>) = List.foldBack (fun e a ->  e.ToString()+(a:string)) cs ""
implode2 ['a';'b';'c']  ;;


/// Reversed implosion
let implodeRev (cs:List<char>) = List.fold (fun (acc:string) x  -> x.ToString()+acc) "" cs
implodeRev ['a';'b';'c']  ;;


(* Exercise 4.3 *)

/// Without function composition
let toUpper s = implode( List.map (fun x -> System.Char.ToUpper x) (explode s))
toUpper "abGdf";;

//// With backward function composition and pipe-forward
let toUpper2 s = explode s |> List.map (fun x -> System.Char.ToUpper x) |> implode
toUpper2 "abGdf";;


(* Exercise 4.4 *)

/// Checks whether given string is a palindrome
let palindrome s = 
    if (explode >> implodeRev) s = s then true else false

palindrome "anna";;
palindrome "ann";;



(* Exercise 4.5 *)

/// The Ackerman function. Expects a non-negative input.

let rec ack (m, n) = 
    match (m, n) with
    | (0, n) -> n + 1
    | (m, 0) -> ack (m - 1, 1)
    | (m, n) -> ack (m - 1, ack (m, n - 1))
ack (3, 11);;


(* Exercise 4.6 *)

/// Helper function given in assignment description
let time f =
    let start = System.DateTime.Now in
    let res = f () in
    let finish = System.DateTime.Now in
    (res, finish - start)
let timeAck = time (fun () -> ack (3,11));;



/// Time a function, which is given a single argument
let timeArg1 f a = time (fun () -> f a);;
timeArg1 ack (3,11);;


(* Exercise 4.7 *)

/// Run a function with two arguments, (1) a falling integer index (down to 1)
/// and (2) a generic argument, which also defines the return type.
let rec downto1 f n e = 
    match f,n,e with
    |f,0,e -> e
    |f,n,e -> f (n,downto1 f (n-1) e) 
let something = downto1 (fun (x,y) -> x%2 :: y) 5 [];;

/// Using downto1 calculates the factorial of the given argument.
let downtoFact n = downto1 (fun (x,y)-> x*y) n 1
let fact5 = downtoFact 5;;

/// Build a list containing elements with a function applied to the elements'
/// respective indices.
let doDatListThang g n = downto1 (fun (x,y) -> y @ [g x]) n [] ;;
doDatListThang (fun x -> x + x) 5 ;;

let doDatListThang2 g n = downto1 (fun (x,y) -> g x::y) n [] ;;
doDatListThang2 (fun x -> x + x) 5 ;;

