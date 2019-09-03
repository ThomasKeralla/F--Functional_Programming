open System
module e5

// 5.1

type 'a BinTree = 
      Leaf
    | Node of 'a * 'a BinTree * 'a BinTree

let floatBinTree = Node(43.0, Node(25.0, Node(56.0, Leaf, Leaf),
                                         Leaf),
                              Node(562.0, Leaf,
                                          Node(78.0, Leaf,Leaf)))

let rec inOrder x =
    match x with
    | Leaf -> []
    | Node(n,l,Leaf) -> inOrder l @ [n]
    | Node(n,Leaf,r) -> [n] @ inOrder r 
    | Node(n,l,r) -> inOrder l @ [n] @ inOrder r
let res1 = inOrder floatBinTree;;

// 5.2
let rec mapInOrder x f =
   match x with
   | Leaf -> Leaf
   | Node(n,l,Leaf) -> Node(f n,mapInOrder l f,Leaf)
   | Node(n,Leaf,r) -> Node(f n, Leaf, mapInOrder r f)
   | Node(n,l,r) -> Node(f n, mapInOrder l f, mapInOrder r f)
let res2 = mapInOrder floatBinTree (fun x -> x * 2.0) 

let rec foldInOrder f acc tree =
    match tree with
    | Leaf -> acc
    | Node(n,l,Leaf) -> f n (foldInOrder f acc l) 
    | Node(n,Leaf,r) -> f (foldInOrder f acc r) n
    | Node(n,l,r) -> f (foldInOrder f acc r) (f n (f acc (foldInOrder f acc l)))

let res3 = foldInOrder (fun n a -> a + n) 0.0 floatBinTree

let res3 = foldInOrder (fun n a -> if n > a then a+n else n ) 0.0 floatBinTree

type aExp =            (*Arithmetical expressions*)
    | N of int           (*numbers*)
    | V of string        (*variables*)
    | Add of aExp*aExp (*addition*)
    | Mul of aExp*aExp (*multiplication*)
    | Sub of aExp*aExp (*subtraction*)

