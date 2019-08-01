module a4

(* Assignment 5.1-5.3
 * ========================================================================= *)


(* -------------------------------------------------------------- *)
(* Helper functions and definitions from slides and exercise text *)
(* -------------------------------------------------------------- *)

/// The binary tree implementation from the slides from Lecture 5.
type 'a BinTree =
      Leaf
    | Node of 'a * ('a BinTree) * ('a BinTree)

/// Counts leafs and nodes (as a tuple) in a given BinTree
let rec countNodes t =
    let (.+.) (l1,n1) (l2,n2) = (l1+l2,n1+n2 + 1) // NOTE +1 node count here
    match t with
    | Leaf           -> (1,0)
    | Node (_, l, r) -> (countNodes l) .+. (countNodes r)
// let _ = countNodes : 'a BinTree -> int * int

/// Traverses a given BinTree in pre-order
let rec preOrder = function
    | Leaf           -> []
    | Node (x, l, r) -> x :: preOrder l @ preOrder r
// let _ = preOrder : 'a BinTree -> 'a list

let floatBinTree = Node(43.0, Node(25.0, Node(56.0, Leaf, Leaf),
                                         Leaf),
                              Node(562.0, Leaf,
                                          Node(78.0, Leaf,Leaf)))
// let _ = floatBinTree : float BinTree


(* -------------------------------------------------------------- *)
(* End of helper definitions                                      *)
(* -------------------------------------------------------------- *)

let testTree = Node (43, Node(25, Node (56, Leaf, Leaf), Leaf), Node (562, Node(100, Leaf, Leaf), Node (78, Leaf, Leaf)))

(* Exercise 5.1 *)

/// Traverse the given tree in-order and collect nodes' elements
let rec inOrder t =
  match t with 
    | Leaf -> []
    | Node (n, Leaf, r) -> [n] @ inOrder r
    | Node (n, l, Leaf) -> inOrder l @ [n]
    | Node (n, l, r)    -> inOrder l @ [n] @ inOrder r
inOrder testTree;;
inOrder floatBinTree;;

let _ = inOrder : 'a BinTree -> 'a list


(* Exercise 5.2 *)

/// Map a function, f, over a given BinTree, t, in order
let rec mapInOrder f t = 
  match t with 
    | Leaf -> Leaf
    | Node (n, l, Leaf) -> Node (f n, mapInOrder f l, Leaf)
    | Node (n, Leaf, r) -> Node (f n, Leaf, mapInOrder f r)
    | Node (n,l,r)      -> Node (f n, mapInOrder f l, mapInOrder f r) 
mapInOrder (fun x -> x + 1) testTree

let _ = mapInOrder : ('a -> 'b) -> 'a BinTree -> 'b BinTree


(* Exercise 5.3 *)

/// Fold a function, f, over a given BinTree, t, in order
let rec foldInOrder f a t = 
  match t with 
    | Leaf -> a
    | Node (n, l, Leaf) -> f n (foldInOrder f a l)
    | Node (n, Leaf, r) -> f (foldInOrder f a r) n
    | Node (n, l, r)    -> f (foldInOrder f a r) (f n (f a (foldInOrder f a l)))

foldInOrder (fun n a -> a + n) 0.0 floatBinTree;;
foldInOrder (fun n a -> a - n) 0.0 floatBinTree;;

let _ = foldInOrder : ('a -> 'b -> 'b) -> 'b -> 'a BinTree -> 'b


(* Assignments 5.4-5.6
 * ========================================================================= *)

(* ------------------ *)
(* Helper definitions *)
(* ------------------ *)

/// A type alias for a map that stores the state of an executing
/// programme('s scope).
type state = Map<string,int>

/// Update a variable's state
let update = Map.add
let _ = update : string -> int -> state -> state

/// Lookup a variable's state in the given state, and return an option-type
/// containing it if it exists, else None.
let lookup = Map.tryFind
let _ = lookup : string -> state -> int option

(* ------------------------- *)

/// Abstract syntax for arithmetical expressions
type aExp = N of int
          | V of string
          | Add of aExp * aExp
          | Mul of aExp * aExp
          | Sub of aExp * aExp
          | Inc of aExp
/// Abstract syntax for boolean expressions
type bExp = TT
          | FF
          | Eq of aExp * aExp
          | Lt of aExp * aExp
          | Neg of bExp
          | Con of bExp * bExp
/// Abstract syntax for statements
type stm = Ass of string * aExp
         | Skip
         | Seq of stm * stm
         | ITE of bExp * stm * stm
         | While of bExp * stm
         | IT of bExp * stm
         | RU of bExp * stm

/// Interpret an arithmetic expressions
let rec A a s =
    match a with
    | N n        -> n
    | V x        -> Map.find x s
    | Add(a1,a2) -> A a1 s + A a2 s
    | Mul(a1,a2) -> A a1 s * A a2 s
    | Sub(a1,a2) -> A a1 s - A a2 s
    | Inc(a1)    -> A (Add(a1, N 1)) s

let _ = A : aExp -> state -> int

/// Interpret a boolean expression
let rec B b s =
    match b with
    | TT        -> true
    | FF        -> false
    | Eq(a1,a2) when a1 = a2  -> true
    | Eq(a1,a2) when a1 <> a2 -> false
    | Lt(a1,a2) when a1 < a2  -> true
    | Lt(a1,a2) when a1 > a2  -> false
    | Neg b -> not (B b s)
    |Con (s1,s2) when B s1 s && B s2 s -> true 
    |Con (s1,s2) when B s1 s <> true || B s2 s <> true  -> false 

let _ = B : bExp -> state -> bool

/// Interpret an Abstract Syntax Tree (AST)
let rec I stm s =
    match stm with
    | Ass (x,a)         -> update x a s
    | Skip              -> s
    | Seq (stm1,stm2)   -> I stm2 (I stm1 s)
    | While (b,stm1)    -> if B b s then I stm1 s else I Skip s
    | ITE (b,stm1,stm2) -> if B b s then I stm1 s else I stm2 s
    | IT (b,stm1)       -> if B b s then I stm1 s else I Skip s // If-Then
    | RU (b,stm1)       -> if B b s then I Skip s else I (RU(b,stm1)) (I stm1 s) // Repeat-Until

let _ = I : stm -> state -> state

//Example abstract syntax tree from slide 29
let fac_args = Map.ofList [("x", N 4)]
let fac = Seq(Ass("y", N 1),
              While(Neg(Eq(V "x", N 0)),
                    Seq(Ass("y", Mul(V "x", V "y")),
                        Ass("x", Sub(V "x", N 1)))
              )
          )
I fac fac_args;;

(*
  Code compiles but is not correct. Have been sitting with it for a long time so would love 
  some good feedback (as you always give) to understand where we mess up
*)

