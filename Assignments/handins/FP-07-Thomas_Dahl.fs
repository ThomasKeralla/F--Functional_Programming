open System.Windows.Forms.VisualStyles.VisualStyleElement.TaskbarClock
/// ###########################################################################
/// ######################### Functional  Programming #########################
/// ############################  Assignment  7  ##############################
/// ###########################################################################
/// Author: Example Exampleson <example@itu.dk>
///
/// Notes:
/// - The anonymous let bindings below functions (the
///   `let _ = <some function name> : <some type>` statements) enforce type
///   constraints on functions given in the assignments (i.e. ensure that your
///   functions have the correct types) and are meant as a help to you.
///   If the function cannot be cast to the type specified in the assignment
///   text, a (type mismatch) error will be raised.
/// - The actual exercises start below the sections that have clearly been
///   marked as defining helper functions and type definitions.
/// - Mono does not have any apparent way of enabling tail-recursion
///   elimination, meaning:
///   1. It is way slower than it is supposed to be
///   2. StackOverflows can happen even in continuation-based tail-recursive
///      functions.
module a7

(* Exercise 7.1 (HR 9.1) *)

(* Reproduce the stack after a call, `g 2`, where `g` is the following: *)
 *
 * let xs = [1;2]

 * let rec g = function
 *     | 0 -> xs
 *     | n -> let ys = n::g(n-1)
 *            List.rev ys
 *
 * Write a visualisaion of the stack and heap below:
 *
(*
Below is the stack and heap build for calling g 2. To the left is the stack, after each pop the element is removed from the stackframe.
The heap is build to the right. After each pop the heap is shown, the former list/version of ys is removed by garbage collector.                 


//write it out!!!
pop stack4 -> heap = g0 (ys=[1;2]) xs([1;2]) 
pop stack3 -> heap = g1 (ys=[2;1;1]) xs([1;2])  
pop stack2 -> heap = g2 (ys=[1;1;2;2]) xs([1;2])   
//stack  
push stack4 = g 0 -> heap: g2,g1,g0,xs([1;2]) 
push stack3 = g 1 -> heap: g2,g1,xs([1;2]) 
push stack2 = g 2 -> heap: g2,xs([1;2])
pop stack1 = -> heap: xs([1;2])
push stack1 = xs -> heap:-> none
 *)


(* Exercise 7.2 (HR 9.3) *)

let g (m,n) =
  let rec g' a (n,m) = match (n,m) with
    | (m,0) -> (m+a)
    | (m,n) -> g' (a+m+n) (m,n-1)
  g' 0 (m,n)


(* Exercise 7.3 (HR 9.4) *)

/// Compute the length of a list
let rec length (l,n) = 
    match l with
        |[] -> n
        |x::l -> length (l,n+1)

let y = [1;2;3;4]
//make it nice with inner function




(* HELPERS *)
// fact 12 is the last factorial sequence index that will fit in a 32-bit integer
let xs12 = List.init 1000000 (fun i -> 12)
(* END OF HELPERS *)


(* Exercise 7.4 (HR 9.6) *)
  let rec factA = function
      | (0,m) -> m
      | (n,m) -> factA(n-1,n*m)
 

/// Compute the n'th factorial number using continuations
let rec factC n c = 
    match n with
        |0 -> c 1
        |_ -> factC (n-1) (fun t -> c(t*n) )

factC 10 id;;      


// TODO Write some performance analysis and comparison to factA here...

#time;;
for i in xs12 do let _ = factA (i,i) in ();;
for i in xs12 do let _ = factC i id in ();;
#time;;

(*
--> Timing now on
facA:
> for i in xs12 do let _ = factA (i,i) in ();;
Real: 00:00:00.032, CPU: 00:00:00.032, GC gen0: 0, gen1: 0
val it : unit = ()

> //for i in xs12 do let _ = factC i id in ();;
- #time;;

--> Timing now off

--> Timing now on
facC
> //for i in xs12 do let _ = factA (i,i) in ();;
- for i in xs12 do let _ = factC i id in ();;
Real: 00:00:00.376, CPU: 00:00:00.374, GC gen0: 97, gen1: 0
val it : unit = ()

> #time;;

--> Timing now off

*)

(* Exercise 7.5 (HR 8.6) *)

/// Compute the `nth` number of the Fibonacci sequence using a while loop.
let whFib n = 
    let mutable p = 0
    let mutable pp = 0
    let mutable r = 0
    let mutable index = 0
    if n = 1 || n = 2 then r <- 1 else
    
        while index < n do 
            r <- p+pp
            pp <- p
            p <- r
            if p = 0 then p <-1 else ()
            index <- index + 1
    r



(* Exercise 7.6.1 (HR 9.7.1) *)
let rec fibA n f1 f2 = 
    match n with
    |0 -> 0
    |1 -> f1 
    |_ -> fibA (n-1)(f1+f2) (f1)
   
fibA 5 1 0;;

(* Exercise 7.6.2 (HR 9.7.2) *)
let rec fibC (n:int) (c:int->int) = 
  match n with 
    | 0 -> c 0
    | 1 -> c 1
    | _ -> fibC (n-1) (fun pp -> (fun p -> c(p+pp)) (n))
fibC 10 id


(* HELPERS *)
let xs45 = List.init 1000000 (fun i -> 45)
#time
for i in xs45 do let _ = whFib i in ()
//for i in xs45 do let _ = fibA i 1 0 in ()
//for i in xs45 do let _ = fibC i id in ()
#time
(* END OF HELPERS *)

(* Timing now on
whFib:
Real: 00:00:00.068, CPU: 00:00:00.066, GC gen0: 0, gen1: 0
val it : unit = ()

Timing now off *)
(* --> Timing now on
fibA:
Real: 00:00:00.068, CPU: 00:00:00.084, GC gen0: 0, gen1: 0
val it : unit = ()

--> Timing now off *)
(* --> Timing now on
fibC
Real: 00:00:01.166, CPU: 00:00:01.212, GC gen0: 341, gen1: 0
val it : unit = ()

--> Timing now off 

As seen above the whFib and the fibC has same running times whereas the fibC is much slower.
The reason that the first two has same running time is due to f# performing tail optimization 
to optain same efficiency as a while-loop.  
*)



