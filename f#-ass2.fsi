 
open System 
// F# chapter exercises

//chapter 1

//1.1
let g n =  n + 4;;

//1.2
let h (x:float,y:float) = System.Math.Sqrt((x*x) + (y*y));;

//1.3
let g2 = fun n -> n + 4;;
let h2 = fun (x,y) -> System.Math.Sqrt((x*x) + (y*y));; 

//1.4
let rec f = function
    | 0 -> 0
    | x -> x + f (x - 1)
    ;;

let a = f 3;;

//1.5
let rec fib = function
        | 0 -> 0
        | 1 -> 1
        | n -> (fib (n-1)) + (fib (n-2));;
 
 let rec sum = function 
    | (m,0) -> m
    | (m,n) -> m + sum ((m+1),(n-1))

// 1.6
let rec sum (m,n) =
    match n with
    | 0 -> 0
    | x -> m+sum (m,n-1)
let resSum = sum (2,4)

// chapter 2

//2.1
let f2 x = 
    if (x % 2 = 0 && x % 3 = 0) && x % 5 <> 0 then true else false;;

//2.2
let rec pow (x,y):string = 
    match (x,y) with
    | (x,0) -> ""
    | (x,y) -> x + pow (x,y-1)

//2.3
let isIthChar (str:string,i,ch) = if str.[i] = ch then true else false;;

// 2.4 
let rec occFromIth (str:string,i,ch) =
    let s = String.length str
    let rec inner i j s = 
        match i with
        | 0 -> j
        | i -> if i >= str.Length then j elif str.[i] = ch then inner (i+1) (j+1) s else inner (i+1) j s
    inner i 0 s

let res = occFromIth ("abbbcdbb",5,'b');;

// 2.5
let rec occInString = function
    | ("",ch) -> 0
    | (str,ch) -> if str.[0] = ch then 1 + occInString (str.Substring 1, ch) 
                    else 0 + occInString (str.Substring 1, ch)

let testt = occInString ("abccbc",'c');;

// 2.6
let notDivisible (x,y) = y % x <> 0 

// 2.7

let rec test (a,b,c) = 
    match a with
    | _ when a >= b -> notDivisible (b,c)
    | a -> notDivisible(a,c) && test ((a+1),b,c)

let t = test (3,5,12);; // false
let t2 = test (3,5,11);; // true

let prime n = 
    match n with
    | 1 -> true
    | 2 -> true
    | 3 -> true
    | _ when n < 1 -> false
    | _ -> test (2,(n/2),n)

let rec nextPrime n = if prime (n + 1) then (n + 1) else nextPrime (n + 1) 

// 2.8

// 3.1

type AMPM = AM | PM

type Time = {
    AmPm : AMPM;
    Hour : int;
    Minute : int;
}

let t1 = {AmPm = AM; Hour = 3; Minute = 5}
let t2 = {AmPm = AM; Hour = 5; Minute = 3}
let t3 = {AmPm = PM; Hour = 5; Minute = 3}

let r1 = t1 < t2;; // True
let r2 = t2 < t3;; // True
let r3 = t3 < t1;; // false

// just for fun pattern match solution for records, not needed as records can be compared using operators
// meaning this function is absolutely redundent 
let compareTime x y =
    match x,y with
    | {AmPm=AM;Hour=_;Minute=_},{AmPm=PM;Hour=_;Minute=_} -> true
    | {AmPm=PM;Hour=_;Minute=_},{AmPm=AM;Hour=_;Minute=_} -> false
    | {AmPm=AM;Hour=H;Minute=M},{AmPm=AM;Hour=H';Minute=M'} -> if H < H' then true else H = H' && M < M' 
    | {AmPm=PM;Hour=H;Minute=M},{AmPm=PM;Hour=H';Minute=M'} -> if H < H' then true else H = H' && M < M' 

// solution for comparing time with triples

let compareTime2 x y = 
    match x,y with
    | ("AM",_,_),("PM",_,_) -> true
    | ("PM",_,_),("AM",_,_) -> false
    | ("AM",a,b),("AM",a',b') -> 
        match a with
        | a when a < a' -> true
        | a when a > a' -> false
        | a when a = a' ->
            match b with
            | b -> b < b'
            | _ -> failwith "not comparable input"
    | ("PM",a,b),("PM",a',b') -> 
        match a with
        | a when a < a' -> true
        | a when a > a' -> false
        | a when a = a' ->
            match b with
            | b -> b < b'
            | _ -> failwith "not comparable input"
    | _,_ -> failwith "Not valid input"

// same as above implemented with if statments. Polymorphic as it is comparables

let compareTime3 x y =
    let (ampm,hour,minute) = x
    let (ampm',hour',minute') = y
    if ampm < ampm' then true
    elif ampm = ampm' && hour < hour' then true
    elif ampm = ampm' && hour < hour' && minute < minute' then true
    else false

// infix notation 
let (.<.) t1 t2 = compareTime3 t1 t2

let t4 = ("AM",5,5);;
let t5 = ("AM",5,6);;

let r4 = (.<.) t4 t5;; // false

// For both records and triples it is possible to compare the given arguments directly
// as long as they have the same form eg. t4 < t5 equels true. This is because f#
// uses the order in which the arguments are defined to do 

// 3.2 - add and subtract pounds

// poor readability but works
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


let complex1 = (2,3);;
let complex2 = (6,6);;

let resAdd = (.+.) complex1 complex2;;
let resMul = (.*.) complex1 complex2;;
let resSub = (.-.) complex1 complex2;;
let resDivError = (./.) (0,0);;
let resDiv = (./.) complex1 complex2;;

// MISSING LAST COUPLE OF EXERCISES

// 4.1
let upto n =
    let rec inner n l =
        match n with
        | 0 -> l
        | x -> inner  (x-1) (x::l)
    inner n []

let res1 = upto 10;;

let downto1 n =
    let rec downto1' n' l =
        match n' with
        | 0 -> l
        | x -> downto1' (x-1) ((n-x+1)::l)
    downto1' n []

let res2 = downto1 3;;


let downtocheat n = List.rev (upto n)
let res2e = downtocheat 3;;

// 4.3
let evenN n =
    let rec inner n' ac l =
        match n' with
        | 0 -> l
        | x -> inner (n'-1) (ac-2) (ac::l)
    inner n (n*2) []

let res4 = evenN 10;;

// 4.4
let rec altsum = function
    | [0] -> 0
    | xs -> if List.length xs > 1 then xs.Head - xs.Tail.Head + altsum (xs.Tail.Tail)
                                  else xs.Head  
let res5 = altsum [2;-1;3];;

// 4.5 -> error FS0193: Specified method is not supported.
let rmodd x =
    let rec inner x' c acc =
        match x' with
        | [] -> acc
        | xs -> if c%2 = 0 then inner (xs.Tail) (c+1) (acc@[xs.Head]) else inner xs.Tail (c+1) acc
    inner x 2 []
let res6 = rmodd [1;2;3;4;5;6];;

// 4.6
let rec removeEven = function
    | [] -> []
    | x::xs -> if x%2 = 0 then removeEven xs else [x] @ (removeEven xs)

let res8 = removeEven [1;2;3;4;5];; 

// 4.7 
let multiplicity x xs = 
    let rec inner y' ys' acc =
        match ys' with
        | [] -> acc
        | y::ys -> if y = y' then inner y' ys (acc+1) else inner y' ys acc 
    inner x xs 0
let res7 = multiplicity 2 [1;2;4;2;12;-1;2;8];;

//4.8
let split x =
    let rec split' xs' l1 l2 =
        match xs' with
        | [] -> (l1, l2)
        | x::[] -> (l1@[x] ,l2)
        | x::y::xs -> split' xs (l1@[x]) (l2@[y])
    split' x [] []
let res8 = split [1;2;4;2;12;-1;2;8;9];;

// 4.9 
let rec zip (a, b) =
    match a, b with
    | ([],[]) -> []
    | (x::[], y::[]) -> [(x, y)]
    | (x::xs, y::ys) when xs.Length = ys.Length -> 
        (x, y)::zip (xs,ys)
    | _ -> failwith "Length of lists not equal"
let res9 = zip ([2;3;4],[9;8;7]);;

//4.10
let rec prefix x y =
    match x,y with
    | [],[] -> true
    | x::xs,[] -> true
    | [],y::ys -> false
    | x::xs,y::ys -> if x = y then prefix xs ys else false
let res10 = prefix [1;2;3;4] [1;2;3;4;5];;

//4.11
let count (xs,x) =
    let rec inner xs' x' ac c =
        match xs' with
        | [] -> ac
        | x::xs when x >= c -> if x = x' then inner xs x' (ac+1) x else inner xs x' ac x
        | _ -> failwith "Not a ascending list"
    inner xs x 0 0
let res11a = count ([1;2;1;3],2);;

// alternatively below, which takes advantages of it being an ascending list making it more effective with a large datastructure
//however it does not check whether it actually is an ascending list
let count2 (xs,x) =
    let rec inner xs' x' ac =
        match xs' with
        | [] -> ac
        | x::xs -> if x = x' then inner xs x' (ac+1) elif x < x' && ac = 0 then inner xs x' ac 
                             elif x < x' && ac > 0 then failwith "Not a ascending list" else ac
    inner xs x 0 
let res11a2 = count2 ([1;2;2;3;1],2);;

//4.11b
let insert (xs,x) =
    let rec inner xs' (x':int) l =
        match xs' with
        | [] -> l
        | x::xs -> if x < x' then inner xs x' else l  
    inner xs x [] 


let rec insert (xs, x) =
    match xs with
    | [] -> [x]
    | y::[] -> if x <= y then [x] @ [y] else [y] @ [x]
    | y::ys -> if  x <= y then  x::[y] @ ys else y::(insert (ys, x))
    | _ -> failwith "Get a f.... grip - %A" xs

let list = [1..15];;
let list2 = [1;2;3;4;2;7;8];;

let res11b = insert (list2, 5 );;

let rec intersect (xs,xs') =  
    match xs,xs' with
    | _, [] -> []
    | [], _ -> []
    | x::xs, x'::xs' -> if x=x' then x::intersect (xs,xs') elif x > x' then intersect ((x::xs), xs') else intersect (xs,(x'::xs')) 

let res11c = intersect ([1;1;1;2;2],[1;1;2;4]);; 

let rec plus (xs,xs') =
    match xs,xs' with
    | [],[] -> []
    | _,[] -> xs
    | [],_ -> xs'
    | x::xs,x'::xs' -> if x=x' then x::x'::plus (xs,xs') elif x > x' then x'::plus ((x::xs), xs') else x::plus (xs,(x'::xs')) 

let res11d = plus ([1;1;2;8;10],[1;2;4;5;9]);;

let rec minus (xs,xs') =
    match xs,xs' with
    | [], _ -> []
    | _, [] -> xs 
    | x::xs,x'::xs' -> if x=x' then minus (xs,xs') elif x > x' then minus (x::xs,xs') else x::minus (xs,x'::xs') 

let res11e = minus ([1;1;1;2;2],[1;1;2;3]);;

// 4.12 
let rec sum (p,xs) =
    match xs with 
    | [] -> 0
    | x::xs -> if p x then x+sum (p,xs) else sum (p,xs)

let p x = x > 4;;
let res12 = sum (p,[1;3;5;8])

// 4.13
let smlElement xs =
    match xs with
    | [] -> failwith "given empty list"
    | x::xs -> 
                let rec inner x r =
                    match x with
                    | [] -> r 
                    | x'::xs' -> if x' < r then inner xs' x' else inner xs' r
                inner xs x 
let res13a = smlElement [8;2;4;7;1;12;3];;

let delete (a,xs) = 
    let rec inner (a',xs') acc =
        match xs' with
        | [] -> List.rev acc
        | x::xs' -> if x=a then (List.rev acc) @ xs' else inner (a',xs') (x::acc)
    inner (a,xs) []
let res13b = delete (5,[1;2;3;4;5;5;6]);;

let sorts xs =
        match xs with
        | [] -> failwith "Given empty list"
        | x::[] -> [x]
        | _ -> 
                    let rec inner xs' acc =
                        match xs' with
                        | [] -> acc
                        | _ -> let element = smlElement xs'
                               inner (delete (element,xs')) (acc @ [element])
                    inner xs [] 
let re13c = sorts [8;2;4;7;1;12;3];;

// 4.14
let smlElementOption xs =
    match xs with
    | [] -> None 
    | x::xs -> 
                let rec inner x r =
                    match x with
                    | [] -> Some r 
                    | x'::xs' -> if x' < r then inner xs' x' else inner xs' r
                inner xs x 
let res14 = smlElementOption [8;2;4;7;1;12;3];;

// 4.15
let rec revrev xs = 
    match xs with 
    | [] -> []
    | x::xs -> 
                let rec inner x' =
                    match x' with
                    | [] -> []
                    | y::ys -> (inner ys)@[y]
                revrev xs @ [inner x]
               
let res15 = revrev [[1;2];[1;2;3];[10;12;13]];; 

let revrevSimple x = List.rev (x |> List.map (fun x' -> List.rev x'))
let res15extra = revrevSimple [[1;2];[1;2;3]];;  

// Chapter 5
// 5.1 
let filter f xs = List.foldBack (fun x acc -> if f x then x::acc else acc) xs [] 
let res5a = filter (fun x -> x%2 = 0) [1..8];;

// 5.2
let rec foldRevrev list = List.fold (fun ac x -> (List.fold (fun ac' x' -> x'::ac') [] x)::ac) [] list 
let res5b = foldRevrev [[1;2];[1;2;3];[6;7;8]]; 

// 5.3
let foldSum (p,list) = List.fold (fun acc x -> if p x then acc + x else acc) 0 list

let p x = x > 4;;
let res5c = foldSum (p,[1;3;5;8])

// 5.4
let downto1 f n e = if n > 0 then List.foldBack f [1..n] e else e

let foldFact n = downto1 (*) n 1
let res5d = foldFact 5;;

// not quite right - fix this
let gList g n = downto1 g n []
let res5d2 = gList (fun x -> x) 5;; 

// 5.5
let map = [("a","b");("c","d");("d","a")];;

let areNb c1 c2 m = List.exists (fun (x,y) -> (x=c1 && y=c2) || (x=c2 && y=c1)) m

let res5e = areNb "a" "b" map;;
let res5e2 = areNb "a" "c" map;;

let canBeExtBy m col c =  List.forall (fun x -> not (areNb x c m)) col
let res5e3 = canBeExtBy map ["c"] "a"
let res5e4 = canBeExtBy map ["a";"c"] "b"
let test = canBeExtBy map [] "a";;
let test2 = canBeExtBy map ["b"] "a";;

let extColouring m cols c = List.fold (fun acc x -> if canBeExtBy m x c then ([c]@x) ::acc else x::[c]::acc) [] cols
let res5e5 = extColouring map [] "a" // not working for this case
let res5e6 = extColouring map [["c"]] "a";;
let res5e7 = extColouring map [["b"]] "a";;

// 5.6 -> 5.11 missing

// 5.6
let dom r (A:Set<int>) (B:Set<int>) = 
    Set.fold (fun s x -> if r x && Set.exists r B then Set.add x s else s) Set.empty A
let res5f = dom (fun x -> (x > 0) && (x % 2 = 0)) (Set.ofList [-2;3;4;5;6]) (Set.ofList [-8;1;11]);;

// 6.1 - not sure about the meaning

// 6.2

type Fexpr = 
                | Const of float
                | X
                | Add of Fexpr * Fexpr
                | Sub of Fexpr * Fexpr
                | Mul of Fexpr * Fexpr
                | Div of Fexpr * Fexpr
                | Sin of Fexpr
                | Cos of Fexpr
                | Log of Fexpr
                | Exp of Fexpr

let rec toPostfixString e = 
    match e with
    | Const x       -> string x
    | X             -> " x "
    | Add(fe1,fe2)  -> toPostfixString fe1 + " "+ toPostfixString fe2 + " + "
    | Sub(fe1,fe2)  -> toPostfixString fe1 + " "+ toPostfixString fe2 + " - "
    | Mul(fe1,fe2)  -> toPostfixString fe1 + " "+ toPostfixString fe2 + " * "
    | Div(fe1,fe2)  -> toPostfixString fe1 + " "+ toPostfixString fe2 + " / "
    | Sin fe        -> toPostfixString fe  + " "+ " Sin "
    | Cos fe        -> toPostfixString fe  + " Cos "
    | Log fe        -> toPostfixString fe  + " Log "
    | Exp fe        -> toPostfixString fe  + " Exp "

let f = Add (Add (Const 2.1, Cos (Const 3.1)),Add (Const 2.1, Cos (Const 3.1)))

toPostfixString f;;            

// 6.3 - missing

//6.4
type  BinTree<'a,'b> = 
    | Leaf of 'a 
    | Node of BinTree<'a,'b> * 'b * BinTree<'a,'b>;;

let leafVals t =
    let rec inner t' acc=
        match t' with
        | Leaf x-> x::acc
        | Node (l,n,r) ->  (inner l acc) @ (inner r acc)
    Set.ofList (inner t [] )

let t1 = Node(Node(Leaf 1,"cd",Node(Leaf 8,"hd",Leaf 5)),"ab", Leaf 3)
let res6d = leafVals t1;;

let nodeVals t =  
    let rec inner t' acc=
            match t' with
            | Leaf x-> acc
            | Node (l,n,Leaf _) -> inner l (n::acc )
            | Node (Leaf _,n,r) -> inner r (n::acc)
            | Node (l,n,r) -> (inner l acc) @ (inner r acc) @ [n]
    Set.ofList (inner t [] )

let res6e = nodeVals t1

let vals t = (leafVals t, nodeVals t)
let res6f = vals t1 

// 6.5

type AncTree =  | Unspec
                | Info of AncTree * string * AncTree

let nodeAnctree = Info(Info(Info(Unspec,"Verner",Unspec),"Erik",Unspec),"Thomas",Info(Info(Unspec,"Mogens", Unspec),"Kirsten", Info(Unspec,"Agnete",Unspec)))

                                                                                        
let maleAnc t = 
    let rec inner t' collect =
        match t' with
        | Unspec -> []
        | Info (l,n,r) -> if collect then n:: (inner l true) @ (inner r false) else inner l true @ inner r false
    List.tail (inner t true)
let res6g = maleAnc nodeAnctree

let femaleAnc t =
    let rec inner t' collect =
        match t' with
        | Unspec -> []
        | Info (l,n,r) -> if collect then n:: (inner l false) @ (inner r true) else inner l false @ inner r true
    List.tail (inner t true)
let res6h = femaleAnc nodeAnctree

type BinTree<'a> =  | Leaf
                    | Node of BinTree<'a> * 'a * BinTree<'a>  

let bintree1 = Node(Node(Node(Leaf,-3,Leaf),0,Node(Leaf,2,Leaf)),5,Node(Leaf,7,Leaf))                


let rec deleteSml t = 
    match t with 
    | Node (Leaf,x,Leaf) -> (x,Leaf)
    | Node (Leaf,x,r) -> (x,r)
    | Node (l,x,r) -> let (a,b) = deleteSml l
                      if b = Leaf then (a,Node(Leaf,x,r)) else (a,Node(b,x,r))
    |_ -> failwith "error"
let test = deleteSml bintree1


let rec deleteElement t e = 
    match t with 
    | Node (Leaf,x,Leaf) -> if x = e then Leaf else t
    | Node (Leaf,x,r) -> if x = e then r elif e > x then deleteElement r e else t
    | Node (l,x,Leaf) -> if x = e then l elif e < x then deleteElement l e else t
    | Node (l,x,r) -> if x = e then (let (y,z) = deleteSml r 
                                    Node (l,y,z))
                               elif e > x then Node(l,x,deleteElement r e) else Node(deleteElement l e,x,r) 
    | _ -> failwith "error" 
let res6i = deleteElement bintree1 5
                                 
// 6.7 missing

// 6.8
type Instruction = 
                    | ADD 
                    | SUB
                    | MULT
                    | DIV
                    | SIN
                    | COS
                    | LOG
                    | EXP
                    | PUSH of float

type Stack =  float list  

exception StackException of string

/// Interpret the execution of a single instruction on the given Stack
let intpInstr (s :Stack) (i :Instruction) :Stack = 
    match i,s with
    | ADD, s1::s2::s  -> (s1+s2)::s
    | SUB, s1::s2::s  -> (s1-s2)::s
    | MULT, s1::s2::s -> (s1*s2)::s
    | DIV, s1::s2::s  -> (s1/s2)::s
    | ADD, s1::s      -> s1::s
    | SUB, s1::s      -> s1::s
    | MULT, s1::s     -> s1::s
    | DIV, s1::s      -> s1::s
    | SIN, s1::s      -> (System.Math.Sin s1) ::s
    | COS, s1::s      -> (System.Math.Cos s1) ::s
    | LOG, s1::s      -> (System.Math.Log s1) ::s
    | EXP, s1::s      -> (System.Math.Exp s1) ::s
    | PUSH(f1), s     -> f1::s
    | _,_ -> raise (StackException "Pass valid arguments you .... exception")


let s1 = [1.2;3.0;5.6]:Stack

let s2 = intpInstr s1 ADD
let s3 = intpInstr s2 SUB
let pushStack = intpInstr s3 (PUSH (3.0));;

/// Interpret the execution of a programme (which is defined as a list of
/// `Instruction`s as per the exercise text)
let intpProg (is:Instruction List) = 
    let rec intpProgInner list stack =
        match list,stack with
            | [],f::stack -> f
            | i::list,stack -> intpProgInner list (intpInstr stack i)
    intpProgInner is []
    
let list = [PUSH(1.0);PUSH(2.0);PUSH(3.0);PUSH(127.0);MULT;ADD;SUB];;

intpProg list;;

// 6.9
type Company<'a> = Node of 'a * (Company<'a> list) * int

let rec extract (Node(n,l,i)) = 
    (n,i)::List.collect extract l
  
let c1 = Node ("alpha",[],100)  
let c2 = Node ("beta",[],12)
let c3 = Node ("zeta",[],18)
let c4 = Node ("sales", [], 22)
let c5 = Node ("add",[],8)
let c6 = Node ("HR",[c4;c5],1000)
let c7 = Node ("Master", [c1;c2;c3;c6],400)
let res6dep = extract c7

//  --> Chapter 7 <--

//7.1 -> exercises too sad to spent time on
module Vector =
    type Vector = V of {x:float;y:float} 
    type Vector with 
    static member (~-.) (V {x;y})             = V {-x;-y}
    static member (+.) (V {x;y})  (V {x2;y2}) = V {x+x2;y+y2}
    static member (-.) v1     v2              = v1 +. ~-. v2
    static member ( *.) a         (V {x;y})   = V {a*x;a*y}
    static member (&.) (V {x;y})  (V {x2;y2}) = x*x2 + y*y2
    let norm (V {x;y})              = sqrt (x*x + y*y)
    let make (x,y)                  = V {x;y}
    let coord (V {x;y})             = (x,y)

let c = Vector.make (2.0,3.0)

//  --> Chapter 8  <--
// 8.1
let mutable x = 1;;
let mutable y = (x,2);;
let mutable z = y;;
x <- 7;;
print ();; 
y <- (10,10)
print ();;

let print ( )=
    printfn "x = %d" x 
    let (a,b) = y
    printfn "Y = (%d,%d)" a b
    let (c,d) = z 
    printfn "z = (%d,%d)" c d

// 8.2
// The declarations are accepted because the mutable list is polymorphic and is assigned to a type at run time 
//when the first element is added

// 8.3
// dont like to draw

// 8.5
let gcdLoop (x,y) = 
    let mutable m = x
    let mutable n = y
    let mutable h = 0
    while m <>0 do 
        h <- m; m <- n % m ; n <- h
    n

gcdLoop (36,116)     

// 8.6 
let fibLoop n =
    let mutable count = n-2
    let mutable pre = 1
    let mutable prepre = 0
    let mutable list = pre::prepre::[]
    while count > 0 do
        list <- (pre+prepre)::list
        let holder = pre
        pre <- prepre + pre 
        prepre <- holder
        count <- count - 1 
    List.rev list
    
fibLoop 8;;

let printFibList n = printfn "fibonacci result: %A" (fibLoop n)
printFibList 9

//8.7 -> 8-9 missing

//  --> chapter 9  <--
// 9.1

// 9.2 
// n functions as a accumulator and the fact that it has been implemented with a loop above

// 9.3
let rec iterativeSum (m,n) acc = if n <> 0 then iterativeSum (m,n-1) acc+m else acc
let resiteSum = iterativeSum (2,4) 0

let rec continuationSum (m,n) f =
    if n = 0 then f 0
    else continuationSum (m,n-1) (fun x -> f (m+x))
let resCsum = continuationSum (2,4) id

// 9.4
let listLength l =
    let rec inner l acc = 
        match l with
        | [] -> acc 
        | x::xs -> inner xs acc+1
    inner l 0    
let resLlength = listLength [1;1;1;1]

// 9.5
let rec itFold f acc l =
    match l with
    | [] -> acc
    | x::xs -> itFold f (acc + (f x)) xs

let resFold = itFold (fun x -> x+1) 0 [1;1;1]

// 9.6 
let conFact n = 
  let rec inner n' c = 
    match n' with
    | 0 -> c 1
    | x -> c (inner (x-1) (fun x' -> x' * n')) in
  inner n (fun x -> x)
let resConFact = conFact 5

// 9.7 -> hacky but works
let rec fibA n =
    let rec inner n' (a1,a2) =
        match n' with
        | -2 -> 0
        | -1 -> 1
        | 0 -> a1+a2
        | x -> inner (x-1) (a2,a1+a2)
    inner (n-2) (0,1)   
let resFibA = fibA 1

// cant figure this one out
let fibC n =
    let rec inner n' c =
        match n' with
        | 0 -> c 1
        | x -> c inner (x-1) (fun x -> x) c inner (x-2) (fun x' -> x') in
    inner n (fun x y -> x + y)   

let fibC2 n =
    let rec inner n' acc c =
        if n' = 0 then acc 
        else inner (n'-1) acc (fun x -> acc + x + inner (n'-1) acc c)
    inner n id

//  --> chapter 10  <--

// 10.1 --> in seperate file

// --> Chapter 11 <--

// 11.1
let oddSeq = 
    Seq.filter (fun x -> x % 2 <> 0) (Seq.initInfinite (fun x -> x))
let odSeq2 = seq [1 .. 2 .. 99]
let print n seq = printfn "nth = %d" (Seq.item n seq)
let pOddInfi = print 5 oddSeq

// 11.2
let factSeq n = 
    let se = seq [1;1]
    let rec inner indx s = 
       let val1 = Seq.item (indx-1) s 
       let val2 = Seq.item (indx) s
       let newval = (val1+val2) * val2
       if val2 > n then s 
       else
            inner (indx+1) (Seq.append s (Seq.singleton newval))
    inner 1 se        
let resFactSeq = factSeq 6

// 11.4
let sublistSeq i n = 
    let init = Seq.init ((i+n)*2) (fun x -> x*2)
    seq {for i in [0..i+n-1] do yield (Seq.item i init) }

let resSubSeq = printfn "%A" (sublistSeq 5 4)
resSubSeq;;

let sublistSeq2 i n = 
    let init = Seq.init ((i+n)*2) (fun x -> x*2)
    seq {for i in [0..i+n-1] do yield! (Seq.map (fun x -> x + i) init) }
let resSub2 = List.ofSeq (sublistSeq2 3 4) 

// 11.5 
let next a b = (a/b+b)/2.0
next 2.0 1.0
let rec iter f x = function
                    | 0 -> x 
                    | n -> iter f (f x) (n-1)             

let iterate f x = Seq.initInfinite (fun i -> iter f x i);;
iterate (next 2.0) 1.0

let w = List.ofSeq (iterate (fun x -> x *2) [1;2;3] 3) 
let convert s = for i in [0..20] do Seq.item i (iterate (fun x -> x*2) )