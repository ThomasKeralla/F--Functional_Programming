
// I hereby declare that all content of this file is made by me, Thomas Høpfner-Dahl

//student bame : Thomas Keralla Høpfner-Dahl
// student id: 2241538686 / thkh
// date of birth : 14.06.1985
// email : thkh@itu.dk


// Question 1

let infSeq3 = Seq.initInfinite (fun x -> 3 * x)

let finSeq3 n =  seq {0 .. n} 

// get back to this
let sumSeq3 n = [0 .. n] |> List.fold (fun acc x -> acc+x) 0 

// Question 1.2

let seqMap2 f s1 s2 =
    seq { for (x,y) in Seq.zip s1 s2 do
yield f x y }

let swap (x,y) = (y,x)

// The function applies f on each pair produced by Seq.zip

// There is a typ mismatch as the function expects 2 sequences but are given lists


let seqMapFix f s1 s2 =
    seq { for (x,y) in Seq.zip (List.toSeq s1) (List.toSeq s2) do
yield f x y }


// question 2


type TrieNode<'a when 'a : equality> = TN of 'a * bool * TrieNode<'a> list

let trie01 = TN('a',false,[TN('n',false,[TN('d',true,[])])])

// Question 2.1 

let trie03 = TN('a',false,[TN('n',true,[TN('d',true,[])]);TN('d',false,[TN('d',true,[])]);TN('t',true,[])])

let trie04 = TN('a',false,[TN('n',true,[TN('d',true,[])]);TN('d',false,[TN('d',true,[])]);TN('t',true,[TN('x',false,[])])])

// the type of trie04 is TrieNode<char>. The type TrieNode is polymorphic as any value that can be compared can be apllies
// eg. TrieNode can be used with eg. integers as long as int is used at every node to describe 
// the node

exception TrieError of string

// Question 2.2

let numLetters tn =
        match tn with 
        | TN(_,_,[]) -> 1
        | TN(_,_,x) ->
            let l = x
            let rec inner t acc =
                match t with 
                | [] -> acc + 1
                | TN(_,_,x')::x -> inner x acc + inner x' acc
            inner l 0


let rec numWords tn =
    match tn with 
    | TN(_,true,[]) -> 1
    | TN(_,false,[]) -> 0
    | TN(_,true,x) -> 
        let l = x 
        let rec inner l acc =
            match l with
            | [] -> acc
            | (TN(_) as node):: l -> inner l (acc + (numWords node) )
        inner l 1
    | TN(_,false,x) -> 
        let l = x 
        let rec inner l acc =
            match l with
            | [] -> acc
            | (TN(_) as node):: l -> inner l (acc + (numWords node) )
        inner l 0

let toList tn acc =
    match tn with 
    | (TN(x,y,[]) as node ) -> acc
    | (TN(x,y,z) as node) ->  
        let rec inner z acc = 
            match z with 
            | [] -> acc 
            | (TN(x',y',z') as node)::z -> inner z acc @ inner z' ((x,y)::acc )
            | (TN(x',y',[]) as node)::z' ->  (x',y')::acc
        inner z acc 


let exists ls t =
    let m = List.empty
    for i in ls do
        let rec inner t i m =
            match t with
            | TN(x,true,[]) -> if x = i then true::m else m
            | TN(x,true, y) -> if x = i then inner y true::m else inner y m
            | TN(x,false,y) -> inner y m
        m @ (inner t i m)
    List.length m = List.length ls


