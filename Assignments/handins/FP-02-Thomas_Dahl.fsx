module assignment2
open System


//Exercise 1 

let timediff (x, y) (z,q)  =
    (z * 60 + q) - (x * 60 + y) ;; 

//Exercise 2
let minutes (x,y) =
    timediff (0,0) (x,y);;

//Exercise 3
let rec pow (s,n) :string =
    match n with
        |0 -> ""
        |_ -> s + pow (s, n-1)
;;

let rec pow2 =
    function
        |(s,0) -> ""
        |(s,n) -> s + pow(s,n-1)
;;

//Excercise 4
let bin (n,k) =
    n + k
    ;;

//Exercise 5
let rec f = function
    | (0,y) -> y
    | (x,y) -> f(x-1, x*y);;
//1 - type of f is a function og the type int * int -> int
//2 - f terminates with any tuple containing 2 int's
//3 - is 2 (x) equal to 0 -> no -> rec f(1,6) (f(x-1, x*y))
    //is 1 (x) equal to 0 -> no -> rec f(0,6) (f(x-1, x*y))
    //is 0 (x) equal to 0 -> yes -> return 6 (y)

// f (2,3)
// ~> f(2-1, 2*3)
// ~> f((2-1)-1, 1*(2*3))
// ~> f(1-1, 1*6)
// ~> f(0,6)
// result = 6
//4 - x is equal to x! and y is the factor the result of x! will be multiplied 
    //however the basecase y=0 is not covered and x! will give 0, but should be 1




//Exercise 6
let test(c,e) = if c then e else 0;;
// 1 - type of test is a function with type bool * int -> int
// 2 - output will be 0, as c is false then !e, which will trigger the else statement 
// 3 - output = -1 if fact -1 had been able to take negative values
// but the result will be a stack overflow as the case of negative number is not covered
//in fact. had there been a |-_ case with fact being called recursive with n+1 the right result
//would have been outputted.

//Exercise 7
// declare curry f
let curry f = (fun x -> (fun y -> f(x,y)))

// declare uncurry g
let uncurry g = (fun (x,y) -> g x y)
