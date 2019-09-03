
let conFact n = 
  let rec inner n' c = 
    match n' with
    | 0 -> c 1
    | x -> c (inner (x-1) (fun x' -> x' * n')) in
  inner n id 
let resConFact = conFact 2

let doStuff x =
    let first, second = x in
    first + " " + second
let r = doStuff ("a","b")

42 |> (fun x ->
  43 |> (fun y -> 
     x + y |> (fun z -> 
       z)))



listbuilder { 
    let! x = [1..3]
    let! y = [10;20;30]
    return x + y
    } |> printfn "Result: %A" 

type TraceBuilder() =
    member this.Bind(m, f) = 
        match m with 
        | None -> 
            printfn "Binding with None. Exiting."
        | Some a -> 
            printfn "Binding with Some(%A). Continuing" a
        Option.bind f m

    member this.Return(x) = 
        printfn "Returning a unwrapped %A as an option" x
        Some x

    member this.ReturnFrom(m) = 
        printfn "Returning an option (%A) directly" m
        m
    (*
    member this.Zero() = 
        printfn "Zero"
        None
    *)
    member this.Yield(x) = 
        printfn "Yield an unwrapped %A as an option" x
        Some x

    member this.YieldFrom(m) = 
        printfn "Yield an option (%A) directly" m
        m
        
    member this.Delay(f) = 
        printfn "Delay"
        f()
    (*
    member this.Combine (a,b) = 
        match a,b with
        | Some a', Some b' ->
            printfn "combining %A and %A" a' b' 
            Some (a' + b')
        | Some a', None ->
            printfn "combining %A with None" a' 
            Some a'
        | None, Some b' ->
            printfn "combining None with %A" b' 
            Some b'
        | None, None ->
            printfn "combining None with None"
            None
        *)

    member this.Zero() = 
        printfn "Zero"
        None  // failure
    
    member this.Combine (a,b) = 
        printfn "Combining %A with %A" a b
        match a with
        | Some _ -> a  // a succeeds -- use it
        | None -> b    // a fails -- use b instead
        
// make an instance of the workflow 
let trace = new TraceBuilder()

let res =trace { 
                yield 1
                yield 1
                } |> printfn "Result for yield then yield: %A" 

trace { 
    yield 1
    let! x = None
    yield 2
    } |> printfn "Result for yield then None: %A"             

trace { 
    yield 1
    yield 2
    yield 3
    } |> printfn "Result for yield x 3: %A" 

let res2 = listbuilder { 
                        yield 1
                        yield 2
                        } 

listbuilder { 
            yield 1
            yield! [2;3]
            } |> printfn "Result for yield then yield! : %A" 


type ListBuilder() =
    member this.Bind(m, f) = 
        m |> List.collect f

    member this.Zero() = 
        printfn "Zero"
        []
        
    member this.Yield(x) = 
        printfn "Yield an unwrapped %A as a list" x
        [x]

    member this.YieldFrom(m) = 
        printfn "Yield a list (%A) directly" m
        m

    member this.For(m,f) =
        printfn "For %A" m
        this.Bind(m,f)
        
    member this.Combine (a,b) = 
        printfn "combining %A and %A" a b 
        List.concat [a;b]

    member this.Delay(f) = 
        printfn "Delay"
        f()

// make an instance of the workflow 
let listbuilder = new ListBuilder()        

let a = [1;2;3];;
let b = [9;8;7];;
List.concat [a;b;a]

listbuilder { 
    for i in ["red";"blue"] do
        yield i
        for j in ["hat";"tie"] do
            yield! [i + " " + j;"-"]
    } |> printfn "Result for for..in..do : %A" 

let map1 = [ ("A","One"); ("2","Two") ] |> Map.ofList
let map2 = [ ("A","Alice"); ("B","Bob") ] |> Map.ofList

let test = List.concat [map1.TryFind "A";]

let res3 =trace { 
                yield map1.TryFind "A"
                yield map2.TryFind "A"
                } 


let rec f (n,m) =
    match n with 
    | 1 -> m
    | x -> x * (m^x)+ f (n-1,m)    // <-- use System.Math   

let fTail (n,m) =
    let inner (n',m') acc =
        match n with 
        | 1 -> m+acc
        | x -> f (n-1,m) (acc+(x * (m^x))) // <-- use System.Math   
    inner (n,m) 0  

let val = A (10, B (B("Grete",3.14),B("Hans,45.3")) )  

let rec map fA fB ml =
    match ml with 
    | E -> ml
    | A (x,y) -> A (fA x,  map fA fB y) 
    | B (x,y) -> B (fB x, map fA fB y)


let val = A (11, B (B("Grete",3.14),B("Hans",45.3)) )  

let split m1
    let rec inner m1' acc1 acc2 =
        match m1' with
        | E -> acc1 acc2
        | A (x,y)-> inner y (x::acc1) acc2
        | B (x,y) -> inner y acc1 (x::acc2)
    inner m1 [] []    // wrong order reverse list or concat in function 


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
