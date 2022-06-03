module Exam2022
(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but
   it does allow you to work in interactive mode and you can just remove the '='
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs"
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertently
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode.

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2022 =
 *)

(* 1: Grayscale images *)

type grayscale =
    | Square of uint8
    | Quad of grayscale * grayscale * grayscale * grayscale

let img =
    Quad(Square 255uy, Square 128uy, Quad(Square 255uy, Square 128uy, Square 192uy, Square 64uy), Square 0uy)

(* Question 1.1 *)
let rec countWhite =
    function
    | Square s when s = 255uy -> 1
    | Square _ -> 0
    | Quad(s1,s2,s3,s4) -> 
        countWhite s1 + countWhite s2 + countWhite s3 + countWhite s4

(* Question 1.2 *)
let rec rotateRight =
    function
    | Square s -> Square s
    | Quad(s1,s2,s3,s4) -> Quad(rotateRight s4, rotateRight s1, rotateRight s2, rotateRight s3)

(* Question 1.3 *)
let rec map mapper = 
    function
    | Square s -> mapper s
    | Quad(s1,s2,s3,s4) -> Quad(map mapper s1, map mapper s2, map mapper s3, map mapper s4)

let bitmap img = map (fun y -> if y <= 127uy then Square 0uy else Square 255uy) img

(* Question 1.4 *)

let rec fold folder acc = 
    function
    | Square s -> folder acc s
    | Quad(s1,s2,s3,s4) ->
        // FIXME: I'm sorry, for i have sinned
        fold folder (fold folder (fold folder (fold folder acc s1) s2) s3) s4

let countWhite2 img = fold (fun acc y -> if y = 255uy then acc+1 else acc) 0 img 

(* 2: Code Comprehension *)
let rec foo =
    function
    | 0 -> "" // Base case
    | x when x % 2 = 0 -> foo (x / 2) + "0" // When x is even, run foo with half x (int division) and add "0" to the result
    | x when x % 2 = 1 -> foo (x / 2) + "1" // When x is uneven, run foor with half x (int division) and add "1" to the result

let rec bar =
    function
    | [] -> []
    | x :: xs -> (foo x) :: (bar xs)

(* Question 2.1 *)

(*

    Q: What are the types of functions foo and bar?

    A: foo has type "int -> string"
       bar has type "int list -> string list"


    Q: What does the functions foo and bar do.
       Focus on what it does rather than how it does it.

    A: 
        foo takes an integer and returns a string containing that ints binary representation (unless it's 0, then it returns empty string)
        bar takes a list of integers and returns a list of strings which are the binary representation of the integers, in the same order

    Q: What would be appropriate names for functions
       foo and bar?

    A: foo = toBinary or decimalToBinary
       bar = listToBinary or decimalListToBinary

    Q: The function foo does not return reasonable results for all possible inputs.
       What requirements must we have on the input to foo in order to get reasonable results?

    A: foo must be run with a number above 0 (x > 0), as the binary representation of 0 is 0, in this case it just returns empty string which is wrong
    *)


(* Question 2.2 *)


(*
    The function foo compiles with a warning.


    Q: What warning and why?

    A:  The warning is "Incomplete pattern matches on this expression....."
        The warning happens because calculating wether or not pattern match guards are exhaustive is complicated, so F# doesn't try. Therefore even though our pattern match is exhaustive it still tells us that it might not be.

    *)

let rec foo2 = 
    function
    | 0 -> "" // This bug is kept as it must behave the same way for all possible inputs :(
    | x when x % 2 = 0 -> foo (x / 2) + "0"
    | x -> foo (x / 2) + "1" // We've handled all other cases

(* Question 2.3 *)

let bar2 list = List.map (foo) list

(* Question 2.4 *)

(*

    Q: Neither foo nor bar is tail recursive. Pick one (not both) of them and explain why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation.
       You need to make clear what aspects of the evaluation tell you that the function is not tail recursive.
       Keep in mind that all steps in an evaluation chain must evaluate to the same value
       ((5 + 4) * 3 --> 9 * 3 --> 27, for instance).

    A: 
        Let's evaluate a call of bar
        bar [0;1;2;3]
        "" :: (bar [1;2;3])
        "" :: ("1" :: (bar [2;3]))
        "" :: ("1" :: ("10" :: (bar [3])))
        "" :: ("1" :: ("10" :: ("11" :: (bar []))))
        "" :: ("1" :: ("10" :: ("11" :: [])))
        "" :: ("1" :: ("10" :: ["11"]))
        "" :: ("1" :: ["10"; "11"])
        "" :: ["1";"10"; "11"]
        [""; "1";"10"; "11"]

        As observed it's not tail recursive as the result of calling (foo x) cannot be appended to the list as until the computation of (bar xs) is completed. So it depends on it's own call

    Q: Even though neither `foo` nor `bar` is tail recursive only one of them runs the risk of overflowing the stack.
       Which one and why does  the other one not risk overflowing the stack?

    A: 
        bar is the one that risks it.
        foo has an upper limit to it's size, as it uses an int32 and therefore is limited by its size.
        bar on the other hand can overflow the stack as a list can be infinite and it could therefore, in theory, create infinite append calls that await execution

    *)
(* Question 2.5 *)

let fooTail x = 
    let rec aux acc =
        function
        | 0 -> acc |> String.concat ""
        | x when x % 2 = 0 -> aux ("0" :: acc) (x/2)
        // I've fixed the compiler warning, but the original line is here:
        // | x when x % 2 = 1 -> aux ('1' :: acc) (x/2)
        | x -> aux ("1" :: acc) (x/2)
    
    aux [] x

(* Question 2.6 *)
let barTail lst =
    let rec aux c =
        function
        | [] -> c []
        | x :: xs -> 
            let bin = fooTail x
            aux (fun y -> c (bin::y)) xs

            // Could also be written as
            // aux (fun y -> c (fooTail x::y)) xs
            // But that would have the side effect of the calculation being delayed until c is executed, which is not how bar works

    aux id lst

(* 3: Matrix operations *)

type matrix = int [,]

let init f rows cols = Array2D.init rows cols f

let numRows (m: matrix) = Array2D.length1 m
let numCols (m: matrix) = Array2D.length2 m

let get (m: matrix) row col = m.[row, col]
let set (m: matrix) row col v = m.[row, col] <- v

let print (m: matrix) =
    for row in 0 .. numRows m - 1 do
        for col in 0 .. numCols m - 1 do
            printf "%d\t" (get m row col)

        printfn ""


(* Question 3.1 *)

let failDimensions m1 m2 =
    // Note to self the "m2 roms" are in the assignment, i did not misspell that
    failwith (sprintf $"Invalid matrix dimensions: m1 rows = %d{numRows m1}, m1 columns = %d{numCols m1}, m2 roms = %d{numRows m2}, m2 columns = %d{numCols m2}")

(* Question 3.2 *)
let add m1 m2 =
    if numRows m1 <> numRows m2 then failDimensions m1 m2
    if numCols m1 <> numCols m2 then failDimensions m1 m2 // else if isn't necessary as it fails

    init (fun x y -> (get m1 x y) + (get m2 x y)) (numRows m1) (numCols m1)

(* Question 3.3 *)

let m1 = (init (fun i j -> i * 3 + j + 1) 2 3)
let m2 = (init (fun j k -> j * 2 + k + 1) 3 2)

let dotProduct m1 m2 row col =
    let rec aux acc =
        function
        | -1 -> acc
        | x -> aux (acc + (get m1 row x) * (get m2 x col)) (x-1)

    aux 0 (numRows m1)

let mult m1 m2 =
    if numCols m1 <> numRows m2 then failDimensions m1 m2
    if numRows m1 <> numCols m2 then failDimensions m1 m2 // else if isn't necessary as it fails

    init (fun x y -> dotProduct m1 m2 x y) (numRows m1) (numCols m2)

(* Question 3.4 *)
let parInit f rows cols =
    let calc matrix (row, col) =
        async {
            set matrix row col (f row col)
        }

    let m = init (fun _ _ -> 0) rows cols

    // [0..x] includes x, so since indexes start at 0 we remove 1 
    List.allPairs [0..rows-1] [0..cols-1]
    |> List.map (calc m)
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore

    m

(* 4: Stack machines *)

type cmd =
    | Push of int
    | Add
    | Mult

type stackProgram = cmd list

(* Question 4.1 *)

type stack = unit (* replace this entire type with your own *)
let emptyStack _ = failwith "not implemented"

(* Question 4.2 *)

let runStackProgram _ = failwith "not implemented"

(* Question 4.3 *)

type StateMonad<'a> = SM of (stack -> ('a * stack) option)

let ret x = SM(fun s -> Some(x, s))
let fail = SM(fun _ -> None)

let bind f (SM a) : StateMonad<'b> =
    SM (fun s ->
        match a s with
        | Some (x, s') ->
            let (SM g) = f x
            g s'
        | None -> None)

let (>>=) x f = bind f x
let (>>>=) x y = x >>= (fun _ -> y)

let evalSM (SM f) = f (emptyStack ())

let push _ = failwith "not implemented"
let pop _ = failwith "not implemented"

(* Question 4.4 *)

type StateBuilder() =

    member this.Bind(f, x) = bind x f
    member this.Return(x) = ret x
    member this.ReturnFrom(x) = x
    member this.Combine(a, b) = a >>= (fun _ -> b)

let state = new StateBuilder()

let runStackProg2 _ = failwith "not implemented"

(* Question 4.5 *)

open JParsec.TextParser

let parseStackProg _ = failwith "not implemented"
