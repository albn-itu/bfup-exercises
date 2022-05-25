module Exam.FourthQuestion

type Tape<'a> = {
    left: uint64
    right: uint64
    maxLeft: uint64
    maxRight: uint64
    symbolsWritten: Map<(uint64 * uint64), 'a>
}

type Dir = Left | Right

let max a b = if a > b then a else b

let moveLeft tape = 
    match tape.right with
    | 0UL ->
        let newLeft  = tape.left + 1UL
        { tape with left = newLeft; maxLeft = (max newLeft (tape.maxLeft))  }
    | _ -> { tape with right = tape.right - 1UL}

let moveRight tape = 
    match tape.left with
    | 0UL -> 
        let newRight = tape.right + 1UL
        { tape with right = newRight; maxRight = (max newRight (tape.maxRight)) }
    | _ -> { tape with left = tape.left - 1UL}

let getTapeCoord tape = (tape.left, tape.right)

let placeAtHead tape symbol =
    { tape with symbolsWritten = Map.add (getTapeCoord tape) symbol tape.symbolsWritten }

let removeAtHead tape = 
    { tape with symbolsWritten = Map.remove (getTapeCoord tape) tape.symbolsWritten }

let getAtHead tape =
    Map.tryFind (getTapeCoord tape) tape.symbolsWritten

let tapeFromList (osyms: list<'a option>) =
    let osyms = 
        osyms
        |> List.mapi (fun i x -> ((0UL, uint64 i), x)) 
        |> List.filter (fun x -> x |> snd |> Option.isSome)
        |> List.map (fun x -> (x |> fst, x |> snd |> Option.get))

    {
        left = 0UL
        right = 0UL
        maxLeft = 0UL
        maxRight = uint64 (List.length osyms - 1)
        symbolsWritten = Map.ofList osyms
    }

let tapeToList tape = 
    seq {
        for i in tape.maxLeft..0UL do
            yield tape.symbolsWritten.TryFind (i, 0UL)
        
        // Start at 1, as 0,0 has already been done
        for i in 1UL..tape.maxRight do
            yield tape.symbolsWritten.TryFind (0UL, i)
    } |> Seq.toList

let moveHead dir tape =
    match dir with
    | Left -> moveLeft tape
    | Right -> moveRight tape

let readTape tape = 
    getAtHead tape

let writeTape (osym: 'a option) tape =
    match osym with
    | Some sym -> placeAtHead tape sym
    | None -> removeAtHead tape

type Action<'a when 'a : comparison> = 
    Act of Map<'a option, string * 'a option * Dir>

type Program<'a when 'a : comparison> = 
    Prog of Map<string, Action<'a>>

let evalAction (Act(act)) tape : (string * Tape<'a>) option =
    match Map.tryFind (readTape tape) act with
    | Some (label, symbol, direction) -> 
        Some (label, 
            tape
            |> writeTape symbol
            |> moveHead direction
        )
    | None -> None

let evalProgram<'a when 'a : comparison> (prog: Program<'a>) startLabel tape : (string * Tape<'a>) option =
    let prog = 
        match prog with
        | Prog(prog) -> prog
    
    let rec aux label tape =
        match Map.tryFind label prog with
        | Some act ->
            match evalAction act tape with
            | Some (newLabel, newTape) -> aux newLabel newTape
            | None -> None
        | None -> Some (label, tape)

    aux startLabel tape

let bind f =
    function
    | None -> None
    | Some (a,b) -> f a b

let (>>=) x f = bind f x

// We should supply string and tape to a, then the output goes to b

let (|>>|) (a: (string -> Tape<'a> -> (string * Tape<'a>) option)) (b: (string -> Tape<'a> -> (string * Tape<'a>) option)) : (string -> Tape<'a> -> (string * Tape<'a>) option) = 
    (fun x y -> a x y |> bind b)

let combineProgramEvaluations (evaluationFunctions: (string -> Tape<'a> -> (string * Tape<'a>) option) list) : string -> Tape<'a> -> (string * Tape<'a>) option =
    let rec aux funcs func =
        match funcs with
        | [] -> func
        | (f::funcs) -> aux funcs (f |>>| func)
    
    match evaluationFunctions with
    | [] -> fun x y -> None
    | (f::funcs) -> aux funcs f



let unaryAddition startL midL1 midL2 endL = 

    let move c dir label = Map.add c (label, c, dir)
    let write old n dir label = Map.add old (label, n, dir)

    let moveToEnd startLabel endLabel = 
        Map.empty |>
        move (Some true) Right startLabel |>
        write (Some false) (Some true) Left endLabel |>
        Act

    let moveToStart startLabel endLabel =
        Map.empty |>
        move (Some true) Left startLabel |>
        move None Right endLabel |>
        Act

    let start nextLabel endLabel = 
        Map.empty |>
        write (Some false) None Right endLabel |>
        write (Some true) None Right nextLabel |>
        Act

    Map.empty |>
    Map.add startL (start midL1 endL) |>
    Map.add midL1 (moveToEnd midL1 midL2) |>
    Map.add midL2 (moveToStart midL2 endL) |>
    Prog

let testUnary =
    let mkUnary x = [for i in 1..x do yield Some true
                     yield Some false]

    function
    | [] -> []
    | xs -> List.collect mkUnary xs

let addUnaryNumbers eval numbers =
    let tape = tapeFromList (testUnary numbers)
    
    printfn "%A" tape

    eval "start" tape |>
    Option.map snd |>
    Option.map tapeToList

let addTwoUnaryNumbers a b =
    addUnaryNumbers 
        (evalProgram (unaryAddition "start" "mid1" "mid2" "end"))
        [a; b]

let addThreeUnaryNumbers a b c =
    addUnaryNumbers 
        (evalProgram (unaryAddition "start" "mid1" "mid2" "end") |>>|
         evalProgram (unaryAddition "end" "mid1" "mid2" "start"))
        [a; b; c]

let addSeveralUnaryNumbers numbers = 
    let eval1 = evalProgram (unaryAddition "start" "mid1" "mid2" "end")
    let eval2 = evalProgram (unaryAddition "end" "mid1" "mid2" "start")

    let evals = 
        [for i in 2..List.length numbers 
            do yield if i % 2 = 0 then eval1 else eval2] |>
        combineProgramEvaluations

    addUnaryNumbers evals numbers


printfn "%A" (addTwoUnaryNumbers 5 10)


let test () =
    let prog = Prog (Map.ofList [("runningLabel", Act (Map.ofList [(Some false, ("runningLabel", Some false, Right)); (Some true, ("runningLabel", Some true, Right)); (None, ("endLabel", None, Left))]))])
        
    tapeFromList [Some true; Some true; Some true; Some false; Some true; Some true; Some true; Some true; Some true; Some false]
    |> moveHead Right
    |> moveHead Right
    |> moveHead Right
    |> evalProgram prog "runningLabel"

    