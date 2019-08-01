/// ###########################################################################
/// ######################### Functional  Programming #########################
/// ############################  Assignment  8  ##############################
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

module a8

(* -------------------------------------------------------------- *)
(* Helper functions and definitions from slides and exercise text *)
(* -------------------------------------------------------------- *)

/// A datatype for representing a Binary Tree whose node elements are of
/// generic type.
type BinTree<'a> = Node of 'a BinTree * 'a * 'a BinTree
                 | Leaf

let lt3   = Node (Node (Node (Leaf, 1, Leaf), 2, Leaf), 3, Leaf)
let rt3   = Node (Leaf, 3, Node (Leaf, 2, Node (Leaf, 1, Leaf)))

/// Count the number of `Node`s in a tree.
let rec count = function
    | Leaf          -> 0
    | Node(lt,n,rt) -> count lt + count rt + 1;;

/// Count the number of `Node`s in a tree (using continuation function)
let rec countC t c =
    match t with
    | Leaf          -> c 0
    | Node(tl,_,tr) -> countC tl (fun vl -> countC tr (fun vr -> c (vl+vr+1)))
let _ = countC : BinTree<'a> -> (int -> 'b) -> 'b

/// Generate a big list using continuation functions (used in 8.3)
let rec bigListK n k =
    if n <= 0
    then k []
    else bigListK (n-1) (fun res -> 1::k(res))

let _ = bigListK : int -> ('a list -> int list) -> int list

(* -------------------------------------------------------------- *)
(* End of helper definitions                                      *)
(* -------------------------------------------------------------- *)


(* Exercise 8.1 (HR 9.8) *)

/// Count the number of Nodes in given Binary Tree using an accumulator
/// argument.
/// This function is not tail-recursive since it will to first complete the
/// recursive call to one subtree before traversing the other.
let rec countA a t = 
    match t with
    | Leaf          -> a
    | Node(lt,n,rt) -> (a+1) + (countA a lt + countA a rt)

let _ = countA : int -> BinTree<'a> -> int


(* Exercise 8.2 (HR 9.9) *)

/// Count the number of Nodes in given Binary Tree using an accumulator
/// argument as well as a continuation function.
let rec countAC t n c = 
    match t with
    | Leaf -> c 0
    | Node(lt,x,rt) -> countAC lt n c + (fun x' -> x' + n) 1 + countAC rt n c

let _ = countAC : BinTree<'a> -> int -> (int -> 'b) -> 'b


(* Exercise 8.3 (HR 9.10) *)

// Write your analysis of the StackOverflow here

(* Because continuation is used a large chain of lists is created on the heap without the garbage collector 
   being able to collect the elements as they are in use. Thus a stackOverflow is created as the heap runs out of the allocated space given by f# 

*)


(* Exercise 8.4 (HR 9.11) *)

/// Generate an unbalanced BinTree where the value of all nodes is the height
/// of the remainder of the tree and all right subtrees are leafs.
///
/// I.e. leftTree 5 id should generate the tree:
///
///           5
///          / \
///         4
///        / \
///       3
///      / \
///     2
///    / \
///   1
///  / \
///
let rec leftTree n c = 
    match n with 
    | 1 -> c (Node ( Leaf, 1, Leaf )) 
    | n -> c (leftTree (n - 1) (fun x' -> Node(x',n, Leaf)))

let _ = leftTree : int -> (BinTree<int> -> 'a) -> 'a


/// Generate an unbalanced BinTree where the value of all nodes is the height
/// of the remainder of the tree and all left subtrees are leafs.
let rec rightTree n c = 
    match n with 
    | 1 -> c (Node ( Leaf, 1, Leaf )) 
    | n -> c (rightTree (n - 1) (fun x' -> Node(Leaf,n, x')))

let _ = rightTree : int -> (BinTree<int> -> 'a) -> 'a

// 1: Use leftTree and rightTree functions to show the stack limit of
//    count and countA
// 2: Test the performance of countC and countAC

// 1.
count (leftTree 130000 id);; // runs
count (leftTree 131000 id) // stack overflow
count (rightTree 130000 id) // runs
count (rightTree 131000 id) // stack overflow

countAC (leftTree 104000 id) 0 id // runs
countAC (leftTree 105000 id) 0 id // stack overflow
countAC (rightTree 104000 id) 0 id // runs
countAC (rightTree 105000 id) 0 id // stack overflow
(*
the tree size for count is higher than countAC, 130000 and 104000 respectively
*)

// 2
#time;;
countC (leftTree 100000 id) id;;
countC (rightTree 100000 id) id;;
countAC (leftTree 100000 id) 0 id;;
countAC (rightTree 100000 id) 0 id;;
#time;;

(*
running the above code on my machine yields the following results, the results are the first run after starting a 'fresh' environment:

    countC (leftTree 100000 id) id;;


Real: 00:00:00.035, CPU: 00:00:00.050, GC gen0: 4, gen1: 1
val it : int = 100000

    countC (rightTree 100000 id) id;;


Real: 00:00:00.027, CPU: 00:00:00.030, GC gen0: 3, gen1: 0
val it : int = 100000

    countAC (leftTree 100000 id) 0 id;;


Real: 00:00:00.023, CPU: 00:00:00.020, GC gen0: 2, gen1: 0
val it : int = 100000

    countAC (rightTree 100000 id) 0 id;;


Real: 00:00:00.019, CPU: 00:00:00.020, GC gen0: 1, gen1: 0
val it : int = 100000

Thus as expedcted the acumulating countAC method seems to be the faster one.
*)






(* Exercise 8.5 (HR 11.1) *)

/// Generate an infinite sequence of odd numbers.
let oddNums =  Seq.filter( fun i -> if i % 2 = 0 then false else true) (Seq.initInfinite id) 
    
printfn "%A" oddNums;;



(* Exercise 8.6 (HR 11.2) *)

/// Generate an infinite sequence of factorial numbers

let factNum' =
    let mutable j = 1
    let u i =
        if i = 0 then j <- 1
        else if i = 1 then j <- 1
        else if i = 2 then j <- 2
        else
            j <- i * j
        j
    Seq.initInfinite (fun i -> u i)
factNum'

//let factNum = 
    //let mutable cache = 0
    //let r = Seq.filter( fun i -> if i = 0 then cache = 1 else cache = cache * i) (Seq.initInfinite id) 
    //Seq.initInfinite (fun i -> cache)

let _ = factNum : seq<int>

