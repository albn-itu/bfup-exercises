module LExam

open JParsec.TextParser
(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but
   it does allow you to work in interactive mode and you can just remove the '='
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs"
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertantly
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode.

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2021_2 =
 *)

(* 1: Binary lists *)

(* Question 1.1 *)

type binList<'a, 'b> =
    | Nil
    | Cons1 of 'a * binList<'a, 'b>
    | Cons2 of 'b * binList<'a, 'b>

let rec length =
    function
    | Nil -> 0
    | Cons1 (_, ls)
    | Cons2 (_, ls) -> 1 + length ls

(* Question 1.2 *)
let split ls =
    let rec aux c1 c2 =
        function
        | Nil -> (c1 [], c2 [])
        | Cons1 (t, ls) -> aux (fun l -> c1 (t :: l)) c2 ls
        | Cons2 (t, ls) -> aux c1 (fun l -> c2 (t :: l)) ls

    aux id id ls

let length2 ls =
    let rec aux acc1 acc2 =
        function
        | Nil -> (acc1, acc2)
        | Cons1 (_, ls) -> aux (acc1 + 1) acc2 ls
        | Cons2 (_, ls) -> aux acc1 (acc2 + 1) ls

    aux 0 0 ls


(* Question 1.3 *)
let map f g lst =
    let rec aux c =
        function
        | Nil -> c Nil
        | Cons1 (x, ls) -> aux (fun l -> c (Cons1(f x, l))) ls
        | Cons2 (y, ls) -> aux (fun l -> c (Cons2(g y, l))) ls

    aux id lst

(* Question 1.4 *)

let filter f g lst =
    let holds func pack elem c =
        match func elem with
        | true -> fun l -> c (pack (elem, l))
        | false -> c

    let rec aux c =
        function
        | Nil -> c Nil
        | Cons1 (x, ls) -> aux (holds f Cons1 x c) ls
        | Cons2 (y, ls) -> aux (holds g Cons2 y c) ls

    aux id lst



(* Question 1.5 *)

let fold f g acc lst =
    let rec aux acc =
        function
        | Nil -> acc
        | Cons1 (x, ls) -> aux (f acc x) ls
        | Cons2 (x, ls) -> aux (g acc x) ls

    aux acc lst

(* 2: Code Comprehension *)
let rec foo xs ys =
    match xs, ys with
    | [], ys -> ys
    | xs, [] -> xs
    | x :: xs, y :: ys when x < y -> x :: (foo xs (y :: ys))
    | x :: xs, y :: ys -> y :: (foo (x :: xs) ys)

let rec bar =
    function
    | [] -> []
    | [ x ] -> [ x ]
    | xs ->
        let (a, b) = List.splitAt (List.length xs / 2) xs
        foo (bar a) (bar b)

(* Question 2.1 *)

(*

    Q: What are the types of functions foo and bar?

    A:
        foo = ('a list -> 'a list -> 'a list) when 'a: comparison
        bar = ('a list -> 'a list) when 'a: comparison

    Q: What does the function bar do.
       Focus on what it does rather than how it does it.

    A: Sorts a given list

    Q: What would be appropriate names for functions
       foo and bar?

    A:
        foo = insertionSortAppend, no its merge
        bar = sort or insertionSort, no its mergeSort

    Q: What would be appropriate names of the values a and b in bar.


    A:
        a = sorted
        b = toInsert
        I really have no clue

    *)


(* Question 2.2 *)


(*
    The code includes the keyword "and".

    Q: What function does this keyword serve in general
       (why would you use "and" when writing any program)?

    A: `and` is the keyword for mutual recursive functions. Its used when 2 functions are defined in terms of eachother in such a way that neither functions without the other


    Q: What would happen if you removed it from this particular program and
       replaced it with a standard "let"
       (change the line "and bar = " to "let rec bar = ")?
       Explain why the program either does or does not work.

    A: The program works, because foo is not defined in terms of bar, and does not need it to be defined to function. bar needs foo though. The and keyword serves no purpose here

    *)

(* Question 2.3 *)
let foo2 xs ys =
    List.unfold
        (function
        | [], [] -> None
        | [], y :: ys -> Some(y, ([], ys))
        | x :: xs, [] -> Some(x, ([], xs))
        | x :: xs, y :: ys when x < y -> Some(x, (xs, y :: ys))
        | x :: xs, y :: ys -> Some(y, (x :: xs, ys)))
        (xs, ys)
(*
    Compare foo to foo2
    foo2 seems to be more efficient in most cases, as it is "tail recursive" in the sense that it is not "not tail-recursive" like foo is
    BUT foo2 is much less efficient if one of the lists is significantly longer than the other, as foo2 still has to go through every element in both lists to finish
*)

(* Question 2.4 *)

(*

    Q: Neither foo nor bar is tail recursive. Pick one (not both) of them and explain why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation.
       You need to make clear what aspects of the evaluation tell you that the function is not tail recursive.
       Keep in mind that all steps in an evaluation chain must evaluate to the same value
       ((5 + 4) * 3 --> 9 * 3 --> 27, for instance).

    A: Let's take foo as an example. with the list [1;3;5] and [2;4;6]
    foo [1;3;5] [2;4]
    x = 1, y = 2 -> 1 < 2 -> 1 :: (foo [3;5] (2 :: [4]))    -> 1 :: (foo [3;5] [2;4])
    x = 3, y = 2 -> 3 > 2 -> 2 :: (foo (3 :: [5]) [4])      -> 1 :: (2 :: (foo [3;5] [4]))
    x = 3, y = 4 -> 3 < 4 -> 3 :: (foo [5] (4 :: []))       -> 1 :: (2 :: (3 :: (foo [5] [4])))
    x = 5, y = 4 -> 5 > 4 -> 4 :: (foo (5 :: []) [])        -> 1 :: (2 :: (3 :: (4 :: (foo [5] []))))
    xs, []                -> [5]                            -> 1 :: (2 :: (3 :: (4 :: [5])))
                                                            -> 1 :: (2 :: (3 :: [4;5]))
                                                            -> 1 :: (2 :: [3;4;5])
                                                            -> 1 :: [2;3;4;5]
                                                            -> [1;2;3;4;5]

    You should focus on the elements after the last arrow. These show that each add to the list, cannot finish until the execution of the next foo is done.
    Therefore the function is not tail recursive

    *)
(* Question 2.5 *)

let fooTail xs ys =
    let rec aux acc xs ys =
        match xs, ys with
        | xs, [] -> (List.rev acc) @ xs
        | [], ys -> (List.rev acc) @ ys
        | x :: xs', y :: _ when x < y -> aux (x :: acc) xs' ys
        | _ :: _, y :: ys' -> aux (y :: acc) xs ys'

    aux [] xs ys


(* Question 2.5 *)

let barTail lst =
    let rec aux c =
        function
        | [] -> c []
        | [ x ] -> c [ x ]
        | xs ->
            let (left, right) = List.splitAt (List.length xs / 2) xs
            aux (fun vl -> aux (fun vr -> c (fooTail vl vr)) right) left

    aux id lst


(* 3: Approximating square roots *)

(* Question 3.1 *)

let closestPerfectSquare (n: int) : int =
    let n = float n
    let x = System.Math.Floor(System.Math.Sqrt(n))

    match System.Math.Sqrt(n) - x with
    | 0.0 -> int n
    | _ ->
        let above = ((x + 1.0) * (x + 1.0))
        let below = (x * x)

        let diff1 = above - n
        let diff2 = n - below

        if diff1 = diff2 || diff1 > diff2 then
            int below
        else
            int above

let approxSquare x num =
    let y = System.Math.Sqrt(float (closestPerfectSquare x))
    let x = float x

    let rec aux acc =
        function
        | 0 -> acc
        | n -> aux ((x / acc + acc) / 2.0) (n - 1)

    aux y num


(* Question 3.2 *)

let quadratic a b c num =
    let sq = approxSquare (b * b - 4 * a * c) num

    let b = float b
    let a = float a

    ((-b + sq) / (2.0 * a), (-b - sq) / (2.0 * a))

(* Question 3.3 *)

let parQuadratic (eqs: (int * int * int) list) numProcesses num =
    let calc lst =
        async {
            return
                lst
                |> List.map (fun (a, b, c) -> quadratic a b c num)
        }

    eqs
    |> List.splitInto numProcesses
    |> List.map calc
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.fold (fun st l -> st @ l) []

(* Question 3.4 *)
let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
let spaces = many whitespaceChar <?> "spaces"
let spaces1 = many1 whitespaceChar <?> "space1"

let (.>*>.) a b = (a .>> spaces) .>>. b
let (.>*>) a b = (a .>> spaces) .>> b
let (>*>.) a b = (a .>> spaces) >>. b


let solveQuadratic str num =
    let square = pstring "x^2" <?> "square"

    let endPart =
        pchar '=' <?> "equals" >*>. pchar '0' <?> "zero"
        .>> pchar '\n'

    let delim = choice [ pchar '+'; pchar '-' ] .>*>. pint32


    let aParser = pint32 <?> "first int" .>*> square

    let bParser = aParser >*>. delim .>*> pchar 'x' <?> "x"

    let cParser = bParser >*>. delim .>*> endPart <?> "end"

    let str = sprintf "%s\n" str

    let getInt p =
        match run p str |> getSuccess with
        | '-', x -> -x
        | _, x -> x

    let a = run aParser str |> getSuccess

    quadratic a (getInt bParser) (getInt cParser) num

(* 4: Rational numbers *)

(* Question 4.1 *)

type rat = Rat of int * int

(* Question 4.2 *)
let rec gcd n d =
    match n, d with
    | n, 0 -> n
    | n, d -> gcd d (n % d)

let neg n = -1 * n

let simplify (n: int) (d: int) =
    let g = gcd (System.Math.Abs n) (System.Math.Abs d)
    Rat(n / g, d / g)

let mkRat n d =
    match n, d with
    | _, 0 -> None
    | n, d when (n < 0 && d < 0) || d < 0 -> Some(simplify (neg n) (neg d))
    | n, d -> Some(simplify n d)


let ratToString (Rat (n, d)) = sprintf "%d / %d" n d

(* Question 4.3 *)

let plus (Rat (a, b)) (Rat (c, d)) = mkRat (a * d + b * c) (b * d)
let minus (Rat (a, b)) (Rat (c, d)) = mkRat (a * d - b * c) (b * d)
let mult (Rat (a, b)) (Rat (c, d)) = mkRat (a * c) (b * d)
let div (Rat (a, b)) (Rat (c, d)) = mkRat (a * d) (b * c)

(* Question 4.4 *)

type SM<'a> = SM of (rat -> ('a * rat) option)
let ret x = SM(fun st -> Some(x, st))

let bind (SM m) f =
    SM (fun st ->
        match m st with
        | None -> None
        | Some (x, st') ->
            let (SM g) = f x
            g st')

let (>>=) m f = bind m f
let (>>>=) m n = m >>= (fun () -> n)
let evalSM (SM f) s = f s

let smPlus rat =
    SM(fun st -> plus st rat |> Option.map (fun s -> ((), s)))

let smMinus rat =
    SM(fun st -> minus st rat |> Option.map (fun s -> ((), s)))

let smMult rat =
    SM(fun st -> mult st rat |> Option.map (fun s -> ((), s)))

let smDiv rat =
    SM(fun st -> div st rat |> Option.map (fun s -> ((), s)))

(* Question 4.5 *)

(* You may solve this exercise either using monadic operators or
        using computational expressions. *)

type StateBuilder() =

    member this.Bind(x, f) = bind x f
    member this.Zero() = ret ()
    member this.Return(x) = ret x
    member this.ReturnFrom(x) = x
    member this.Combine(a, b) = a >>= (fun _ -> b)


let state = new StateBuilder()

let rec calculate list =
    match list with
    | [] -> ret ()
    | (r, op) :: ls -> op r >>>= calculate ls