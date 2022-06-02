module Exam

(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but
   it does allow you to work in interactive mode and you can just remove the '='
   to make the project compile work again.

   Do not remove the line (even though that does work) because you may inadvertantly
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode.

   Alternative, keep the line as is, but load ExamInteractive.fsx into the interactive environment
   *)
(* module Exam2020 = *)

(* 1: Insertion sort *)

(* Question 1.1 *)

let rec insert x =
    function
    | [] -> [ x ]
    | l :: ls when x <= l -> x :: l :: ls
    | l :: ls -> l :: (insert x ls)

let rec insertionSort =
    function
    | [] -> []
    | x :: ls -> insert x (insertionSort ls)

(* Question 1.2 *)

let insertTail x ls =
    let rec aux acc =
        function
        | [] -> (List.rev (x::acc))
        | l :: ls when x <= l -> (List.rev acc) @ (x :: l :: ls)
        | l :: ls -> aux (l::acc) ls

    aux [] ls

let insertionSortTail ls =
    let rec aux acc =
        function
        | [] -> acc
        | x :: ls -> aux (insertTail x acc) ls

    aux [] ls

(* Question 1.3 *)

(*
    Q: Why are the higher-order functions from the List library
    not a good fit to implement insert?

    A: The greatest problem here would be that you have to stop computations, so you dont wanna loop through the entire list if the right case is right in the beginning. You would have to start checking if both places are the right fit or something.
    *)

let insertionSort2 ls =
    List.fold (fun state x -> insert x state) [] ls

(* Question 1.4 *)

let insertBy f x ls =
    let rec aux acc =
        function
        | [] -> (List.rev (x::acc))
        | l :: ls when f x <= f l -> (List.rev acc) @ (x :: l :: ls)
        | l :: ls -> aux (l::acc) ls

    aux [] ls

let insertionSortBy f ls =
    List.fold (fun state x -> insertBy f x state) [] ls

(* 2: Code Comprehension *)
let rec foo x =
    function
    | y :: ys when x = y -> ys
    | y :: ys -> y :: (foo x ys)

let rec bar x =
    function
    | [] -> []
    | xs :: xss -> (x :: xs) :: bar x xss

let rec baz =
    function
    | [] -> []
    | [ x ] -> [ [ x ] ]
    | xs ->
        let rec aux =
            function
            | [] -> []
            | y :: ys -> ((foo y >> baz >> bar y) xs) @ (aux ys)

        aux xs

(* Question 2.1 *)

(*

    Q: What are the types of functions foo,  bar, and baz?

    A:
        foo =  'a -> 'a list -> 'a list when 'a: equality
        bar =  'a -> 'a list list -> 'a list list
        baz =  'a list -> 'a list list when 'a: equality



    Q: What do functions foo, bar, and baz do?
       Focus on what they do rather than how they do it.

    A:
        foo removes the first occurence of an element
        bar adds an element to all the lists
        baz gets all permutations


    Q: What would be appropriate names for functions
       foo, bar, and baz?

    A: 
        foo = removeFirstOccurence
        bar = addToAll
        baz = getAllPermutations

    *)


(* Question 2.2 *)


(*
    The function foo generates a warning during compilation:
    Warning: Incomplete pattern matches on this expression.


    Q: Why does this happen, and where?

    A: It happens because the matches is missing a case for then the list is empty


    Q: For these particular three functions will this incomplete
       pattern match ever cause problems for any possible execution of baz?
       If yes, why; if no, why not.

    A: It causes an issue in foo if the element does not exist in the list, as it crashes

    *)

let rec foo2 x =
    function
    | [] -> []
    | y :: ys when x = y -> ys
    | y :: ys -> y :: (foo x ys)



(* Question 2.3 *)

(*
    In the function baz there is a sub expression foo y >> baz >> bar y

    Q: What is the type of this expression

    A: 'a list -> 'a list list: when 'a comparison


    Q: What does it do? Focus on what it does rather than how it does it.

    A: It simplifies the expression. Instead of having to provide the argument to foo, we can pipe the result through to baz then to bar, which creates a simpler function

    *)

(* Question 2.4 *)

let bar2 x ls = List.foldBack (fun y st -> (x::y)::st) ls []

(* Question 2.5 *)

let baz2 =
    function
    | [] -> []
    | [ x ] -> [ [ x ] ]
    | xs ->
        List.fold (fun st y -> st @ (foo y >> baz >> bar y) xs) [] xs

(* Question 2.6 *)

(*

    Q: The function foo is not tail recursive. Why?

    A: it depends on it self
        foo 3 [1;2;3]
        1 :: (foo 3 [2;3]) 
        1 :: (2 :: (foo 3 [2;3])) 
        1 :: (2 :: ([])) 
        1 :: [2]
        [1;2]

        foo depends on its own recursive call to finish before it can add its elements to the result 

    *)

let fooTail x lst =
    let rec aux c =
        function
        | [] -> c []
        | y :: ys when x = y -> c ys
        | y :: ys -> aux (fun l -> c (y::l)) ys

    aux id lst

(* 3: Rock Paper Scissors *)

(* Question 3.1 *)

type shape = Rock | Paper | Scissors 
type result = P1Win | P2Win | Draw

let mkShape =
   function
   | "rock"     -> Rock 
   | "paper"    -> Paper 
   | "scissors" -> Scissors
   | s          -> failwith (sprintf "invalid shape: %s" s) 

let shapeToString =
   function
   | Rock     -> "rock"
   | Paper    -> "paper"
   | Scissors -> "scissors"

let resultToString =
    function
    | P1Win -> "playerOneWin"
    | P2Win -> "playerTwoWin"
    | Draw         -> "draw"


let rps s1 s2 = 
    match s1,s2 with
    | Rock, Scissors -> P1Win
    | Scissors, Paper -> P1Win
    | Paper, Rock -> P1Win
    | _ when s1 = s2 -> Draw
    | _ -> P2Win

(* Question 3.2 *)
type strategy = (shape * shape) list -> shape

let parrot s : strategy =
    function
    | [] -> s
    | (_, s2) :: _ -> s2

let beatingStrat moves : shape =
    let rec count (r, p, s) =
        function
        | [] -> (r,p,s)
        | (_, Rock) :: m -> count (r+1, p,s) m
        | (_, Paper) :: m -> count (r, p+1,s) m
        | (_, Scissors) :: m -> count (r, p,s+1) m

    match count (0,0,0) moves with
    | (r,p,s) when s >= p && s >= r -> Rock
    | (r,p,s) when r >= p && r >= s -> Paper
    | _ -> Scissors

let roundRobin shapes : strategy =
    let mutable ms = shapes

    let rec aux () =
        match ms with 
        | [] ->
            ms <- shapes
            aux ()
        | x :: xs ->
            ms <- xs
            x
    
    fun _ -> aux ()

(* Question 3.3 *)

(*

    Q: It may be tempting to generate a function that calculates your
       point tuple after n rounds and then use Seq.initInfinite to
       generate the sequence. This is not a good solution. Why?

    A: seq.infinite only works for functions that know nothing of the previous step

    *)

let bestOutOf strat1 strat2 = 
    Seq.unfold 
        (fun (p1moves, p2moves, p1, p2) ->
            let s1 = strat1 p1moves
            let s2 = strat2 p2moves
            let (p1', p2') = 
                match rps s1 s2 with 
                | P1Win -> (p1 + 1, p2)
                | P2Win -> (p1, p2 + 1)
                | Draw  -> (p1, p2)
            Some (
                (p1', p2'), 
                (
                    (s1, s2)::p1moves, 
                    (s2, s1)::p2moves, 
                    p1', p2'
                )))

        ([], [], 0, 0) |>

    Seq.append (Seq.singleton (0, 0))

(* Question 3.4 *)

let playTournament rounds players =
    let rec initRound acc =
        function
        | [] -> (acc, [])
        | [x] -> (acc, [x])
        | x::y::xs -> initRound ((x, y)::acc) xs

    let getRoundWinner ((p1, id1), (p2, id2)) =
        async {
            return 
                match bestOutOf p1 p2 |> Seq.item rounds with
                | (p1w, p2w) when p1w = p2w -> None // draw
                | (p1w, p2w) when p1w > p2w -> Some (p1, id1)
                | _ -> Some (p2, id2)
        }

    let rec aux =
        function 
        | [] -> None
        | [(_, id)] -> Some id // Only one player? he wins
        | players -> 
            let (pairs, rest) = initRound [] players

            pairs 
            |> List.map getRoundWinner
            |> Async.Parallel
            |> Async.RunSynchronously
            |> Array.toList
            |> List.filter Option.isSome
            |> List.map Option.get
            |> (fun lst -> aux (lst @ rest)) // Start over

    aux (List.mapi (fun i player -> (player, i)) players)

(* 4: Revers Polish Notation *)

(* Question 4.1 *)

type stack = int list 

let emptyStack = [] : stack

(* Question 4.2 *)

type SM<'a> = S of (stack -> ('a * stack) option)

let ret x = S(fun s -> Some(x, s))
let fail = S(fun _ -> None)

let bind f (S a) : SM<'b> =
    S (fun s ->
        match a s with
        | Some (x, s') ->
            let (S g) = f x
            g s'
        | None -> None)

let (>>=) x f = bind f x
let (>>>=) x y = x >>= (fun _ -> y)

let evalSM (S f) = f emptyStack

let push x = 
    S(fun s -> Some((), x::s))

let pop =
    S(
        function
        | [] -> None
        | x::s -> Some(x,s) 
    ) 

(* Question 4.3 *)

let write str : SM<unit> =
    S (fun s ->
        printf "%s" str
        Some((), s))

//let write str : SM<unit> =
//    ret(printf "%s" str)


let read =
    let rec aux acc =
        match System.Console.Read() |> char with
        | '\n' when acc = [] -> None
        | c when System.Char.IsWhiteSpace c ->
            acc
            |> List.fold (fun strAcc ch -> (string ch) + strAcc) ""
            |> Some
        | c -> aux (c :: acc)

    S(fun s -> Some(aux [], s))

(*

    Q: Consider the definition of write There is a reason that the definition
       is S (fun s -> printf "%s" str; Some ((), s)) and not just
       ret (printf "%s" str). For a similar reason, in read, we write
       S (fun s -> Some (aux [], s)) and not ret (aux []).
       What is the problem with using ret in both of these cases?

    A: ret's contents is considered a value, and is therefore executed immediatly, not after evalSM is run  

    *)

(* Question 4.4 *)

(* You may solve this exercise either using monadic operators or
        using computational expressions. *)

type StateBuilder() =

    member this.Bind(f, x) = bind x f
    member this.Return(x) = ret x
    member this.ReturnFrom(x) = x
    member this.Combine(a, b) = a >>= (fun _ -> b)

let state = new StateBuilder()

let isInt (str : string) : bool = System.Int32.TryParse str |> fst

let calculateRPN () = 
    let binOp operator =
        state {
            let! x = pop
            let! y = pop
            do! push (operator x y)
        }

    let rec aux () =
        state {
            let! str = read

            match str with
            | None ->
                let! result = pop
                do! write (string result)
                return () 
            | Some "+" -> 
                do! binOp (+)
                return! aux ()
            | Some "-" ->
                do! binOp (fun x y -> y - x)
                return! aux ()
            | Some "*" ->
                do! binOp (*)
                return! aux ()
            | Some s when isInt s -> 
                do! push (int s)
                return! aux ()
            | Some _ -> return! fail
        }

    aux ()