(*
    Assignment 9
    Thomas Høpfner-Dahl thkh@itu.dk
    Based upon: http://www.itu.dk/people/sestoft/bachelor/computationexpression.fs
*)

// Very simple expressions

type expr =
    | CstI of int
    | Prim of string * expr * expr
    | Prim1 of string * expr
    | Prim3 of string * string * expr * expr * expr

let random = new System.Random()

// ------------------------------------------------------------

// Plain evaluator, return type int

let rec eval1 e : int =
    match e with
    | CstI i -> i
    | Prim3(op1, op2, e1, e2, e3) -> 
        let v1 = eval1 e1
        let v2 = eval1 e2
        let v3 = eval1 e3
        let res1 = 
            match op1 with
                | "+" -> v1 + v2
                | "*" -> v1 * v2
                | "/" -> v1 / v2
        let res2 =
            match op2 with 
                | "+" -> res1 + v3
                | "*" -> res1 * v3
                | "/" -> res1 / v3
        res2
    | Prim1(op, e1) ->
        let v1 = eval1 e1
        match op with
            | "ABS" -> if v1 < 0 then v1 * -1 else v1
    | Prim(op, e1, e2) ->
        let v1 = eval1 e1
        let v2 = eval1 e2
        match op with
        | "+" -> v1 + v2
        | "*" -> v1 * v2
        | "/" -> v1 / v2

let opEval op v1 v2 : int =
    match op with
    | "+" -> v1 + v2
    | "*" -> v1 * v2
    | "/" -> v1 / v2

let opEvalSingle op v1 : int =
    match op with 
        | "ABS" -> if v1 < 0 then v1 * -1 else v1

let opEvalTripple op1 op2 v1 v2 v3 =
    let res1 = 
        match op1 with
            | "+" -> v1 + v2
            | "*" -> v1 * v2
            | "/" -> v1 / v2
    let res2 =
        match op2 with 
            | "+" -> res1 + v3
            | "*" -> res1 * v3
            | "/" -> res1 / v3
    res2

let rec eval2 e : int =
    match e with
    | CstI i -> i
    | Prim3 (op1, op2, e1, e2, e3) ->
        let v1 = eval2 e1
        let v2 = eval2 e2
        let v3 = eval2 e3
        opEvalTripple op1 op2 v1 v2 v3
    | Prim1(op, e1) ->
        let v1 = eval2 e1
        opEvalSingle op v1
    | Prim(op, e1, e2) ->
        let v1 = eval2 e1
        let v2 = eval2 e2
        opEval op v1 v2

type IdentityBuilder() =
    member this.Bind(x, f) = f x
    member this.Return x = x
    member this.ReturnFrom x = x

let identM = new IdentityBuilder();;

let rec eval3 e : int =
    match e with
    | CstI i -> identM { return i }
    | Prim3(op1, op2, e1, e2, e3) ->
        identM { 
            let! v1 = eval3 e1 
            let! v2 = eval3 e2 
            let! v3 = eval3 e3
            return! opEvalTripple op1 op2 v1 v2 v3
            }
    | Prim1(op, e1) -> 
        identM { let! v1 = eval3 e1 
                 return! opEvalSingle op v1 }
    | Prim(op, e1, e2) ->
        identM  { let! v1 = eval3 e1
                  let! v2 = eval3 e2
                  return! opEval op v1 v2 }

eval1 (Prim1("ABS", Prim("+", CstI(4), Prim("*", CstI(-3), CstI(5)))))
eval2 (Prim1("ABS", Prim("+", CstI(4), Prim("*", CstI(-3), CstI(5)))))
eval3 (Prim1("ABS", Prim("+", CstI(4), Prim("*", CstI(-3), CstI(5)))))

eval1 (Prim3("+", "*", Prim1("ABS", Prim("+", CstI(2), CstI(-6))), CstI(5), CstI(3)))
eval2 (Prim3("+", "*", Prim1("ABS", Prim("+", CstI(2), CstI(-6))), CstI(5), CstI(3)))
eval3 (Prim3("+", "*", Prim1("ABS", Prim("+", CstI(2), CstI(-6))), CstI(5), CstI(3)))

// ------------------------------------------------------------

// Evaluator that may fail, return type: int option

(* 
    compiling these sometimes bug out on my machine 
    ... though I can't consistently reproduce the behaviour
*)

let rec optionEval1 e : int option =
    match e with
    | CstI i -> Some i
    | Prim1(op, e1) ->
        match optionEval1 e1 with
            | None -> None
            | Some v1 -> 
                match op with
                    | "ABS" -> if v1 < 0 then Some (v1 * -1) else Some (v1)
    | Prim(op, e1, e2) ->
        match optionEval1 e1 with
        | None -> None
        | Some v1 ->
            match optionEval1 e2 with
            | None -> None
            | Some v2 ->
                match op with
                | "+" -> Some(v1 + v2)
                | "*" -> Some(v1 * v2)
                | "/" -> if v2 = 0 then None else Some(v1 / v2)
    | Prim3 (op1, op2, e1, e2, e3) -> 
        match optionEval1 e1 with 
        | None -> None
        | Some v1 -> 
            match optionEval1 e2 with
            | None -> None
            | Some v2 -> 
                match optionEval1 e3 with 
                | None -> None
                | Some v3 ->
                        let res1 = 
                            match op1 with
                                | "+" -> v1 + v2
                                | "*" -> v1 * v2
                                | "/" -> v1 / v2
                        if v2 = 0 then None else Some (res1)
                        let res2 =
                            match op2 with 
                                | "+" -> res1 + v3
                                | "*" -> res1 * v3
                                | "/" -> res1 / v3
                        if v3 = 0 then None else Some (res2)

let opEvalOptTripple op1 op2 v1 v2 v3 =
    let res1 = 
        match op1 with
            | "+" -> v1 + v2
            | "*" -> v1 * v2
            | "/" -> v1 / v2
    if v2 = 0 then None else Some (res1)
    let res2 =
        match op2 with 
            | "+" -> res1 + v3
            | "*" -> res1 * v3
            | "/" -> res1 / v3
    if v3 = 0 then None else Some (res2)

let opEvalOptSingle op v1 : int option =
    match op with
        | "ABS" -> if v1 < 0 then Some (v1 * -1) else Some (v1)

let opEvalOpt op v1 v2 : int option =
    match op with
    | "+" -> Some(v1 + v2)
    | "*" -> Some(v1 * v2)
    | "/" -> if v2 = 0 then None else Some(v1 / v2)
                
let rec optionEval2 e : int option =
    match e with
    | CstI i -> Some i
    | Prim1(op, e1) ->
        match optionEval2 e1 with
            | None -> None
            | Some v1 -> opEvalOptSingle op v1
    | Prim(op, e1, e2) ->
        match optionEval2 e1 with
        | None -> None
        | Some v1 ->
            match optionEval2 e2 with
            | None -> None
            | Some v2 -> opEvalOpt op v1 v2
    | Prim3 (op1, op2, e1, e2, e3) -> 
        match optionEval1 e1 with 
        | None -> None
        | Some v1 -> 
            match optionEval1 e2 with
            | None -> None
            | Some v2 -> 
                match optionEval1 e3 with 
                | None -> None
                | Some v3 ->
                    opEvalOptTripple op1 op2 v1 v2 v3

let optionFlatMap (f : 'a -> 'b option) (x : 'a option) : 'b option =
    match x with
    | None   -> None
    | Some v -> f v;;

type OptionBuilder() =
    member this.Bind(x, f) =
        match x with
        | None   -> None
        | Some v -> f v
    member this.Return x = Some x
    member this.ReturnFrom x = x
 
let optionM = OptionBuilder();;

let rec optionEval3 e : int option =
    match e with
    | CstI i -> optionM { return i }
    | Prim1(op, e1) -> 
        optionM { let! v1 = optionEval3 e1 
                  return! opEvalOptSingle op v1 }
    | Prim(op, e1, e2) ->
        optionM { let! v1 = optionEval3 e1
                  let! v2 = optionEval3 e2
                  return! opEvalOpt op v1 v2 }
    | Prim3(op1, op2, e1, e2, e3) ->
        optionM {
            let! v1 = optionEval3 e1
            let! v2 = optionEval3 e2
            let! v3 = optionEval3 e3
            return! opEvalOptTripple op1 op2 v1 v2 v3
        }


optionEval1 (Prim1("ABS", Prim("+", CstI(4), Prim("*", CstI(-3), CstI(5)))))
optionEval2 (Prim1("ABS", Prim("+", CstI(4), Prim("*", CstI(-3), CstI(5)))))
optionEval3 (Prim1("ABS", Prim("+", CstI(4), Prim("*", CstI(-3), CstI(5)))))

optionEval1 (Prim3("+", "*", Prim1("ABS", Prim("+", CstI(2), CstI(-6))), CstI(5), CstI(3)))
optionEval2 (Prim3("+", "*", Prim1("ABS", Prim("+", CstI(2), CstI(-6))), CstI(5), CstI(3)))
optionEval3 (Prim3("+", "*", Prim1("ABS", Prim("+", CstI(2), CstI(-6))), CstI(5), CstI(3)))


// ------------------------------------------------------------                

// Evaluator that returns a set of results, return type: int Set

let opEvalSet op v1 v2 : int Set =
    match op with
    | "+" -> Set [v1 + v2]
    | "*" -> Set [v1 * v2]
    | "/" -> if v2 = 0 then Set.empty else Set [v1 / v2]
    | "choose" -> if random.NextDouble() > 0.5 then  Set [v1] else Set [v2]

let opEvalSetSingle op v1 : int Set =
    match op with 
        | "ABS" -> if v1 < 0 then Set [v1 * -1] else Set [v1]

let opEvalSetTripple op1 op2 v1 v2 v3 : int Set = 
    let res1 = 
        match op1 with 
        | "+" -> v1 + v2
        | "*" -> v1 * v2
        | "/" -> if v2 = 0 then 0 else v1 / v2
        | "choose" -> if random.NextDouble() > 0.5 then  v1 else v2
    opEvalSet op2 res1 v3

let rec setEval1 e : int Set =
    match e with
    | CstI i -> Set [i]
    | Prim1 (op, e1) ->
        let s1 = setEval1 e1
        let yss = Set.map (fun v1 -> opEvalSetSingle op v1) s1
        Set.unionMany yss
    | Prim(op, e1, e2) ->
        let s1 = setEval1 e1
        let yss = Set.map (fun v1 ->
           let s2 = setEval1 e2
           let xss = Set.map (fun v2 -> opEvalSet op v1 v2) s2
           Set.unionMany xss) s1
        Set.unionMany yss
    | Prim3(op1, op2, e1, e2, e3) ->
        let s1 = setEval1 e1
        let yss = Set.map (fun v1 -> 
            let s2 = setEval1 e2
            let xss = Set.map (fun v2 -> 
                let s3 = setEval1 e3
                let zss = Set.map (fun v3 -> opEvalSetTripple op1 op2 v1 v2 v3) s3
                Set.unionMany zss) s2
            Set.unionMany xss) s1
        Set.unionMany yss


let setFlatMap (f : 'a -> 'b Set) (x : 'a Set) : 'b Set =
    Set.unionMany (Set.map f x);;

type SetBuilder() =
    member this.Bind(x, f) =
        Set.unionMany (Set.map f x)
    member this.Return x = Set [x]
    member this.ReturnFrom x = x
 
let setM = SetBuilder();;

let rec setEval3 e : int Set =
    match e with
    | CstI i -> setM { return i }
    | Prim1(op, e1) -> 
        setM { let! v1 = setEval3 e1 
               return! opEvalSetSingle op v1 }
    | Prim(op, e1, e2) ->
        setM { let! v1 = setEval3 e1
               let! v2 = setEval3 e2
               return! opEvalSet op v1 v2 }
    | Prim3(op1, op2, e1, e2, e3) ->
        setM {
            let! v1 = setEval3 e1
            let! v2 = setEval3 e2
            let! v3 = setEval3 e3
            return! opEvalSetTripple op1 op2 v1 v2 v3
        }

setEval1 (Prim1("ABS", Prim("+", CstI(4), Prim("*", CstI(-3), CstI(5)))))
setEval3 (Prim1("ABS", Prim("+", CstI(4), Prim("*", CstI(-3), CstI(5)))))

setEval1 (Prim3("+", "*", Prim1("ABS", Prim("+", CstI(2), CstI(-6))), CstI(5), CstI(3)))
setEval3 (Prim3("+", "*", Prim1("ABS", Prim("+", CstI(2), CstI(-6))), CstI(5), CstI(3)))

// ------------------------------------------------------------

// Evaluator that records sequence of operators used,
// return type: int trace

type 'a trace = string list * 'a

let opEvalTrace op v1 v2 : int trace =
    match op with
    | "+" -> (["+"], v1 + v2)
    | "*" -> (["*"], v1 * v2)
    | "/" -> (["/"], v1 / v2)
    | "choose" -> (["choose"], if random.NextDouble() > 0.5 then v1 else v2)

let opEvalTraceSingle op v1 : int trace =
    match op with
        | "ABS" -> (["ABS"], if v1 < 0 then v1 * -1 else v1)

let opEvalTraceTripple op1 op2 v1 v2 v3 : int trace =
    let (trace1, res1) = opEvalTrace op1 v1 v2
    let (trace2, res2) = opEvalTrace op2 res1 v3
    (trace1 @ trace2, res2)

    

let rec traceEval1 e : int trace =
    match e with
    | CstI i -> ([], i)
    | Prim1 (op, e1) ->
        let (trace1, v1) = traceEval1 e1
        let (trace2, res) = opEvalTraceSingle op v1
        (trace1 @ trace2, res)
    | Prim(op, e1, e2) ->
        let (trace1, v1) = traceEval1 e1
        let (trace2, v2) = traceEval1 e2
        let (trace3, res) = opEvalTrace op v1 v2
        (trace1 @ trace2 @ trace3, res)
    | Prim3 (op1, op2, e1, e2, e3) ->
        let (trace1, v1) = traceEval1 e1
        let (trace2, v2) = traceEval1 e2
        let (trace3, v3) = traceEval1 e3
        let (trace4, res) = opEvalTraceTripple op1 op2 v1 v2 v3
        (trace1 @ trace2 @ trace3 @ trace4, res)

let traceFlatMap (f : 'a -> 'b trace) (x : 'a trace) : 'b trace =
    let (trace1, v) = x
    let (trace2, res) = f v
    (trace1 @ trace2, res)

type TraceBuilder() =
    member this.Bind(x, f) =
        let (trace1, v) = x
        let (trace2, res) = f v
        (trace1 @ trace2, res)
    member this.Return x = ([], x)
    member this.ReturnFrom x = x
 
let traceM = TraceBuilder();;

let rec traceEval3 e : int trace =
    match e with
    | CstI i -> traceM { return i }
    | Prim1(op, e1) ->
        traceM { let! v1 = traceEval3 e1
                 return! opEvalTraceSingle op v1 }
    | Prim(op, e1, e2) ->
        traceM { let! v1 = traceEval3 e1
                 let! v2 = traceEval3 e2
                 return! opEvalTrace op v1 v2 }
    | Prim3(op1, op2, e1, e2, e3) ->
        traceM {
            let! v1 = traceEval3 e1
            let! v2 = traceEval3 e2
            let! v3 = traceEval3 e3
            return! opEvalTraceTripple op1 op2 v1 v2 v3
        }

traceEval1 (Prim1("ABS", Prim("+", CstI(4), Prim("*", CstI(-3), CstI(5)))))
traceEval3 (Prim1("ABS", Prim("+", CstI(4), Prim("*", CstI(-3), CstI(5)))))

traceEval1 (Prim3("+", "*", Prim1("ABS", Prim("+", CstI(2), CstI(-6))), CstI(5), CstI(3)))
traceEval3 (Prim3("+", "*", Prim1("ABS", Prim("+", CstI(2), CstI(-6))), CstI(5), CstI(3)))



// ------------------------------------------------------------
(* 3 *)

type expr =
    | CstI of int
    | Prim of string * expr * expr
    | Prim1 of string * expr
    | Prim3 of string * string * expr * expr * expr

let random = new System.Random()

type 'a trace = string list * 'a

(* A *)

let opEvalOTA op o1 o2 : (int option) trace =
    let v1 = Option.get o1
    let v2 = Option.get o2
    match op with
    | "+" -> (["+"], Some (v1 + v2))
    | "*" -> (["*"], Some (v1 * v2))
    | "/" -> (["/"], if v2 = 0 then None else Some( v1 / v2))
    | "choose" -> (["choose"], if random.NextDouble() > 0.5 then Some v1 else Some v2)

let rec optionTraceEvalA e =
    match e with
    | CstI i -> ([], Some (i))
    | Prim(op, e1, e2) ->
        let (trace1, v1) = optionTraceEvalA e1
        let (trace2, v2) = optionTraceEvalA e2
        let (trace3, res) = opEvalOTA op v1 v2
        (trace1 @ trace2 @ trace3, res)

type OptionTraceBuilderA() =
    member this.Bind(x,f) =
        let (trace1, v) = x
        let (trace2, res) = f v
        (trace1 @ trace2, res)
    member this.Return x = ([], Some x)
    member this.ReturnFrom x = x

let optionTraceAM = new OptionTraceBuilderA()

let rec optionTraceEvalA_CE e : (int option) trace =
    match e with
    | CstI i -> 
        optionTraceAM {
            return i
        }
    | Prim(op,e1,e2) ->
        optionTraceAM {
            let! v1 = optionTraceEvalA_CE e1
            let! v2 = optionTraceEvalA_CE e2
            return! opEvalOTA op v1 v2
        }

optionTraceEvalA (Prim("+", CstI(4), Prim("*", CstI(-3), CstI(5))))
optionTraceEvalA_CE (Prim("+", CstI(4), Prim("*", CstI(-3), CstI(5))))


(* B *)

let opEvalOTB op v1 v2 : (int trace) option=
    match op with
    | "+" -> Some (["+"], v1 + v2)
    | "*" -> Some (["*"], v1 * v2)
    | "/" -> if v2 = 0 then None else Some (["/"], v1 / v2)
    | "choose" -> Some (["choose"], if random.NextDouble() > 0.5 then v1 else v2)

let rec optionTraceEvalB e : (int trace) option = 
    match e with 
    | CstI i -> Some ([],i)
    | Prim(op, e1, e2) -> 
        let (trace1,v1) = Option.get (optionTraceEvalB e1)
        let (trace2,v2) = Option.get (optionTraceEvalB e2)
        let (trace3, res) =  Option.get (opEvalOTB op v1 v2)
        Some (trace1 @ trace2 @ trace3, res)

type OptionTraceBuilderB() =
    member this.Bind(x,f) =
        let (trace1,v) = x
        let (trace2,res) = f v
        (trace1 @ trace2, res)
    member this.Return x = Some ([],x)
    member this.ReturnFrom x = x

let optionTraceBM = new OptionTraceBuilderB()

let rec optionTraceEvalB_CE e = // : (int trace) option =
    match e with
    | CstI i ->
        optionTraceBM {
            return i
        }
    | Prim(op,e1,e2) ->
        optionTraceBM {
            let (trace1,v1) = Option.get (optionTraceEvalB_CE e1)
            let (trace2,v2) = Option.get (optionTraceEvalB_CE e2)
            let result = opEvalOTB op v1 v2
            if result = None // if the evaluation returns None, then return None, otherwise return the trace and result
            then 
                return! None
            else
                let (trace3,res) = Option.get (opEvalOTB op v1 v2)
                return! (Some (trace1 @ trace2 @ trace3, res))
        }

optionTraceEvalB (Prim("+", CstI(4), Prim("*", CstI(-3), CstI(5))))
optionTraceEvalB_CE (Prim("+", CstI(4), Prim("*", CstI(-3), CstI(5))))


