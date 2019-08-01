/// ###########################################################################
/// ######################### Functional  Programming #########################
/// ############################  Assignment  6  ##############################
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
module a6
open System

(* Exercise 6.1 (HR 6.2) *)

(* -------------------------------------------------------------- *)
(* Helper functions and definitions from slides and exercise text *)
(* -------------------------------------------------------------- *)
/// From Section 6.2 6.2 Symbolic differentiation
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

/// Given a (float) value of the variable, `x`, compute the float value of
/// a given `Fexpr`
let rec compute x = function
    | Const r      -> r
    | X            -> x
    | Add(fe1,fe2) -> compute x fe1 + compute x fe2
    | Sub(fe1,fe2) -> compute x fe1 - compute x fe2
    | Mul(fe1,fe2) -> compute x fe1 * compute x fe2
    | Div(fe1,fe2) -> compute x fe1 / compute x fe2
    | Sin fe       -> System.Math.Sin (compute x fe)
    | Cos fe       -> System.Math.Cos (compute x fe)
    | Log fe       -> System.Math.Log (compute x fe)
    | Exp fe       -> System.Math.Exp (compute x fe)
let _ = compute : float -> Fexpr -> float // constrained to float because Const has a float

/// Differentiate the given `Fexpr`
let rec D = function
    | Const _    -> Const 0.0
    | X          -> Const 1.0
    | Add(fe,ge) -> Add(D fe, D ge)
    | Sub(fe,ge) -> Sub(D fe, D ge)
    | Mul(fe,ge) -> Add(Mul(D fe, ge), Mul(fe, D ge))
    | Div(fe,ge) -> Div(Sub(Mul(D fe,ge), Mul(fe,D ge)),
                        Mul(ge,ge))
    | Sin fe     -> Mul(Cos fe, D fe)
    | Cos fe     -> Mul(Const -1.0, Mul(Sin fe, D fe))
    | Log fe     -> Div(D fe, fe)
    | Exp fe     -> Mul(Exp fe, D fe)
let _ = D : Fexpr -> Fexpr

/// Generate a "simple" textual representation of the given `Fexpr` (simple
/// in the sense that it contains all parentheses)
let rec toString = function
    | Const x       -> string x
    | X             -> "x"
    | Add(fe1,fe2)  -> "(" + (toString fe1) + ")"
                        + " + " + "(" + (toString fe2) + ")"
    | Sub(fe1,fe2)  -> "(" + (toString fe1) + ")"
                        + " - " + "(" + (toString fe2) + ")"
    | Mul(fe1,fe2)  -> "(" + (toString fe1) + ")"
                        + " * " + "(" + (toString fe2) + ")"
    | Div(fe1,fe2)  -> "(" + (toString fe1) + ")"
                        + " / " + "(" + (toString fe2) + ")"
    | Sin fe        -> "(sin " + (toString fe) + ")"
    | Cos fe        -> "(cos " + (toString fe) + ")"
    | Log fe        -> "(log " + (toString fe) + ")"
    | Exp fe        -> "(exp " + (toString fe) + ")"
let _ = toString : Fexpr -> string

/// Extend `Fexpr` with some convenient instance methods to showcase this
/// language facility, which is used in 6.3 (HR 7.2).
type Fexpr with
    member this.Compute x = compute x this
    member this.Differentiate = D this
    override this.ToString() = toString this

(* -------------------------------------------------------------- *)
(* End of helper definitions                                      *)
(* -------------------------------------------------------------- *)


/// Generate a postfix string representation of the given Fexpr
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

let f = Add (Const 2.0, Cos (Const 3.0))

toPostfixString f;;

let _ = toPostfixString : Fexpr -> string


(* Exercise 6.2 (HR 6.8) *)

(* -------------------------------------------------------------- *)
(* Helper functions and definitions from slides and exercise text *)
(* -------------------------------------------------------------- *)
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

(* -------------------------------------------------------------- *)
(* End of helper definitions                                      *)
(* -------------------------------------------------------------- *)

(* Exercise 6.2.1 (HR 6.8.1) *)

/// A type representing the stack
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

let _ = intpInstr : Stack -> Instruction -> Stack

let myStack = [1.2;3.0;5.6]:Stack

let myNewStack = intpInstr myStack ADD
let myNewNewStack = intpInstr myNewStack SUB
let myFail = intpInstr myNewNewStack ADD // explodes!

(* Exercise 6.2.2 (HR 6.8.2) *)

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

let _ = intpProg : Instruction list -> float



(* Exercise 6.2.3 (HR 6.8.3) *)

/// Transform an `Fexpr` to a programme executable by intpProg, by substituting
/// all occurrences of the variable, `x`, with the given floating point numeric
/// value.


let rec trans (fe, x)=
    match fe with
    | Const(v)      -> [PUSH(v)]
    | Add(e1, e2)  -> (trans (e1, x)) @ (trans (e2, x)) @ [ADD]
    | Sub(e1, e2)  -> (trans (e1, x)) @ (trans (e2, x)) @ [SUB]
    | Mul(e1, e2)  -> (trans (e1, x)) @ (trans (e2, x)) @ [MULT]
    | Div(e1, e2)  -> (trans (e1, x)) @ (trans (e2, x)) @ [DIV]
    | Sin(ex)         -> (trans (ex, x)) @ [SIN]
    | Cos(ex)         -> (trans (ex, x)) @ [COS]
    | Log(ex)         -> (trans (ex, x)) @ [LOG]
    | Exp(ex)         -> (trans (ex, x)) @ [EXP]
    | X               -> [PUSH(x)]

let _ = trans : Fexpr * float -> Instruction list

let transTest = trans (Sin(Const(3.0)), 3.0)
let transTest2 = trans (Add(X,(Add(Const(5.0), Const(4.0)))), 5.0)

let test = intpProg transTest2;;
(* Exercise 6.3 (HR 7.2) *)

/// Since the assignment text explicitly says to only upload one file, the
/// signature-file's content is outlined below:

/// module CN

//CN.fsi
[<Sealed>]

type Complex = 
    static member ( + )  : Complex*Complex -> Complex
    static member ( - )  : Complex*Complex -> Complex
    static member ( * )  : float*Complex -> Complex
    static member ( * )  : Complex*Complex -> Complex
    static member ( / )  : Complex*Complex -> Complex

//CN.fs
module CN
type Complex = C of float * float with
    /// Get the sum of two complex numbers
    static member ( + ) (C (r1,c1), C (r2,c2)) = C( r1+r2,c1+c2)
    /// Get the product of two complex numbers
    static member ( - ) (C (r1,c1), C(r2,c2)) = C(r1-r2,c1-c2)
    /// Get the difference between two complex numbers
    static member ( * ) (C (r1,c1), C (r2,c2)) = C((r1*r2)+(r1*c2), (c1*r2)+(c1*c2)) 

    static member ( * ) (x,C(c1,c2)) = C(x*c1,x*c2)
    /// Get the quotient between two complex numbers without recalculating the
    /// squared sum of divisors in resulting tuple
    static member ( / ) (C (r1,c1), C (r2,c2)) = 
        let divisor = (r2*r2 + c2*c2)
        C((r1*r2+c1*c2)/divisor, (r1*r2-c1*c2)/divisor)

// fsharpc -a CN.fsi CN.fs -> CN.dll
