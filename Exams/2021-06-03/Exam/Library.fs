module Exam

open JParsec.TextParser

// Question 1.1

type direction = North | East | South | West
type coord = C of int * int

let createCoord x y =
    C(x,y)

let move dist dir (C(x,y)) =
    match dir with
    | North -> createCoord x (y - dist)
    | South -> createCoord x (y + dist)
    | East -> createCoord (x + dist) y
    | West -> createCoord (x - dist) y

let turnRight =
    function
    | North -> East
    | East -> South
    | South -> West
    | West -> North

let turnLeft =
    function
    | North -> West
    | West -> South
    | South -> East
    | East -> North

// Question 1.2
type position = P of (coord * direction)
type move = TurnLeft | TurnRight | Forward of int

let createPos c dir =
    P(c, dir)

let step (P(c, dir)) =
    function
    | TurnLeft -> createPos c (turnLeft dir)
    | TurnRight -> createPos c (turnRight dir)
    | Forward x -> createPos (move x dir c) dir

let rec walk p =
    function
    | [] -> p
    | m :: ms -> walk (step p m) ms

let walk2 p =
    List.fold step p

let rec path (P(c, _) as p) =
    function
    | [] -> [ c ]
    | Forward _ as m :: ms -> c :: (path (step p m) ms)
    | m :: ms -> path (step p m) ms

let rec path2 p ms =
    let rec aux acc (P(c, _) as p) =
        function
        | [] -> List.rev (c :: acc)
        | Forward _ as m :: ms -> aux (c :: acc) (step p m) ms
        | m :: ms -> aux acc (step p m) ms

    aux [] p ms

(*
    Let's evaluate path (P (C (0, 0), North)) [TurnRight; Forward 10; TurnLeft]
    path (P (C (0, 0), North)) [TurnRight; Forward 10; TurnLeft];
    path (P (C (0, 0), East)) [Forward 10; TurnLeft];
    (P (C (0, 0), East)) :: (path (P (C (10, 0), East)) [TurnLeft])
    (P (C (0, 0), East)) :: (path (P (C (10, 0), North)) [])
    (P (C (0, 0), East)) :: [(P (C (10, 0), North))]
    [(P (C (0, 0), East)); (P (C (10, 0), North))]

    The method is not tail recursive as it depends on the method to finish its recursive calls before it can append it's elements
*)

let rec path3 p ms =
    let rec aux con (P(c, _) as p) =
        function
        | [] -> con [c]
        | Forward _ as m :: ms -> aux (fun l -> con (c::l)) (step p m) ms
        | m :: ms -> aux con (step p m) ms

    aux id p ms

// 2
let foo f =
    let mutable m = Map.empty

    let aux x =
        match Map.tryFind x m with
        | Some y when Map.containsKey x m -> y
        | None -> 
            m <- Map.add x (f x) m; f x
    aux

let rec bar x =
    match x with 
    | 0 -> 0 
    | 1 -> 1
    | y -> baz (y - 1) + baz (y - 2)

and baz = foo bar

(*
    2.1

    foo has type (('a -> 'b) -> 'a -> 'b) when 'a: comparison
    bar has type (int -> int)
    baz has type (int -> int)

    foo is to be run with a method, and saved. It's a memoization layer that saves the result of f and retrieves it if it's already in the map
    bar is the fibonacci sequence
    baz saves it

    the map is mutable such that it can function as a long running memoization layer.
    if we removed it the function would never save anything, apart from the fact that it would error out as <- is not valid without that keyword

    foo = memoize
    bar = fib
    baz = memoFib

    2.2
    The and keyword is for mutually recursive types and methods. It allows definng things that use each other before the other is defined. Such as bar using baz and baz using bar
    If we removed it we couldn't compile as bar would not know how to use baz

    2.3
    It happens in the aux Some match of the foo function
    It happens because F# doesn't even try to consider guards as exhaustive

    No, the guard is redundant as we have already confirmed the key exists
    We check if the key is in the map, then retrieve it, and then check again if the found value is in the map which we have already confirmed that it is

*)

let foo2 f =
    let mutable m = Map.empty

    let aux x =
        match Map.tryFind x m with
        | Some y -> y
        | None -> 
            m <- Map.add x (f x) m; f x
    aux

let rec barbaz x =
    let baz = foo barbaz
    match x with
    | 0 -> 0
    | 1 -> 1
    | y -> baz (y - 1) + baz (y - 2)

(*
    Baz is significantly faster as barbaz creates a new map each time, because it recurses and creates a new stack, therefore not using the purpose of having foo in the first place
*)

let bazSeq = 
    Seq.initInfinite baz

// 3
type element = int list
// This is a good fit as it is infinite, and can only go up to 9 anyway

let elToString (el: element) =
    List.map (fun y -> y.ToString()) el |> String.concat ""

let inline charToInt c = int c - int '0'

let elFromString (s: string) =
    List.ofSeq s |> List.map charToInt

let nextElement elements =
    let rec aux acc cur count =
        function
        | [] -> List.rev (cur :: count :: acc)
        | x :: elements when x = cur -> aux acc cur (count + 1) elements
        | x :: elements -> aux (cur :: count :: acc) x 1 elements

    match elements |> elToString |> elFromString with
    | [] -> []
    | x :: elements -> aux [] x 1 elements

let elSeq element =
    Seq.unfold (fun s ->
        Some(s,nextElement s)
    ) element

let rec elSeq2 element =

    seq {
        yield element
        yield! elSeq2 (nextElement element)
    }
let elParse str =
    let parser = 
        many1 digit <?> "digits"
        .>> pchar '\n' <?> "newline" 

    run parser str |> getSuccess

let elFromString2 (s: string) =
    elParse s |> List.map charToInt

// 4
type ring<'a> = Ring of 'a list * 'a list

let length (Ring(a,b)) = List.length a + List.length b

let ringFromList ls = Ring(List.empty, ls)

let ringToList (Ring(a,b)) = b @ List.rev a

let empty = Ring([], [])
let push x (Ring(xs, ys)) = Ring(xs, x :: ys)
let peek =
    function
    | Ring([], [])    -> None
    | Ring(xs, [])    -> Some (List.head (List.rev xs))
    | Ring(_, y :: _) -> Some y
let pop =
    function
    | Ring([], [])      -> None
    | Ring(xs, [])      -> Some (Ring([], List.tail (List.rev xs)))
    | Ring(xs, _ :: ys) -> Some (Ring(xs, ys))
let cw = 
    function
    | Ring([], [])    -> Ring([], [])
    | Ring([], ys) ->
        let ys' = List.rev ys 
        Ring(List.tail ys', [List.head ys'])
    | Ring(x::xs, ys) -> Ring(xs, x::ys)
    
let ccw =
    function
    | Ring([], [])    -> Ring([], [])
    | Ring(xs, []) ->
        let xs' = List.rev xs 
        Ring([List.head xs'], List.tail xs')
    | Ring(xs, y::ys) -> Ring(y::xs, ys)

(*
let empty<'a> = (List.empty, List.empty) : 'a ring

let push x (a,b) =
    (a, x :: b)

let peek (a, b) =
    match (List.rev a, b) with
    | (x :: _, []) -> Some x
    | (_, x :: _) -> Some x
    | _ -> None

let pop (a,b) =
    match (List.rev a, b) with
    | (_ :: c, []) -> Some ([], c)
    | (a, _ :: c) -> Some (a, c)
    | _ -> None

let cw (a, b) =
    match (a, List.rev b) with
    | ([], []) -> empty
    | ([], x :: c) -> (c, [x])
    | (x :: c, _) -> (c, x::b)

let ccw (a,b) =
    match (List.rev a, b) with
    | ([], []) -> empty
    | (x::c, []) -> ([x], c)
    | (_, x::c) -> (x :: a, c)

*)

type StateMonad<'a, 'b> = SM of ('b ring -> ('a * 'b ring) option)
let ret x = SM (fun st -> Some (x, st))
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

let smLength = SM(fun st -> Some (length st, st))

let smPush a = SM(fun st -> Some ((), push a st))
let smPeek = SM(fun st -> peek st |> Option.map (fun y -> (y, st)))

let smPop = SM(fun st -> peek st |> Option.map (fun y -> (y, pop st |> Option.get)))

let smCW = SM(fun st -> Some ((), cw st))
let smCCW = SM(fun st -> Some ((), ccw st))

type StateBuilder() =
    member this.Bind(x, f)= bind x f
    member this.Zero ()= ret ()
    member this.Return(x)= ret x
    member this.ReturnFrom(x) = x
    member this.Combine(a, b) = a >>= (fun _ -> b)
let state = new StateBuilder()

let ringStep : StateMonad<unit, int> = 
    smLength >>= fun l -> 
    if l > 1 then 
        smPop >>= fun x -> 
        smPop >>= fun y ->
        if (x + y) % 2 = 0 then 
            ret ()
        else
            smPush y >>>=
            smPush x >>>=
            smCCW
    else
        ret ()

let rec iterRemoveSumEven =
    function
    | 0u -> ret ()
    | x  -> ringStep >>>= iterRemoveSumEven (x - 1u)

(*
let ringStep =
    state {
        let! length = smLength

        if length > 1 then
            let! x = smPop
            let! y = smPop

            if (x+y) % 2 = 1 then
                do! smPush y
                do! smPush x
                do! smCCW
    }

let rec iterRemoveSumEven x =
    state {
        if x > 0u then
            do! ringStep
            return! iterRemoveSumEven (x - 1u)
    }
*)

(*
        let ringStep : StateMonad<unit, int> = 
        smLength >>= fun l -> 
        if l > 1 then 
            smPop >>= fun x -> 
            smPop >>= fun y ->
            if (x + y) % 2 = 0 then 
                ret ()
            else
                smPush y >>>=
                smPush x >>>=
                smCCW
        else
            ret ()

    let rec iterRemoveSumEven =
        function
        | 0u -> ret ()
        | x  -> ringStep >>>= iterRemoveSumEven (x - 1u)

    (* You may solve this exercise either using monadic operators or 
        using computational expressions. *)

    type StateBuilder() =

        member this.Bind(x, f)    = bind x f
        member this.Zero ()       = ret ()
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()
        
    let ringStep2 () : StateMonad<unit, int> =
        state {
            let! l = smLength
            if l > 0 then                        <- he made a mistake
                let! x = smPop
                let! y = smPop
                if x % 2 = 0 then
                    return ()
                else
                    do! smPush y
                    do! smPush x
                    do! smCCW
        }
        
    let rec iterRemoveSumEven2 x =
        state {
            if x > 0 then
                do! ringStep2 ()
                do! iterRemoveSumEven2 (x - 1)
        }
*)