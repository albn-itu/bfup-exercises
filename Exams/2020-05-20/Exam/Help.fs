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

let write str : SM<unit> = S (fun s -> printf "%s" str; Some ((), s))

let read =
    let rec aux acc =
        match System.Console.Read() |> char with
        | '\n' when acc = [] -> None
        | c    when System.Char.IsWhiteSpace c -> 
            acc |> List.fold (fun strAcc ch -> (string ch) + strAcc) "" |> Some
        | c -> aux (c :: acc)

    S (fun s -> Some (aux [], s))

    (* 
    
    Q: Consider the definition of write There is a reason that the definition 
       is S (fun s -> printf "%s" str; Some ((), s)) and not just 
       ret (printf "%s" str). For a similar reason, in read, we write 
       S (fun s -> Some (aux [], s)) and not ret (aux []). 
       What is the problem with using ret in both of these cases?

    A: <Your answen goes here>

    *)

(* Question 4.4 *)


    (* You may solve this exercise either using monadic operators or 
        using computational expressions. *)

type StateBuilder() =

    member this.Bind(f, x)    = bind x f
    member this.Return(x)     = ret x
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

[<EntryPoint>]
let main argv =
    calculateRPN () |> evalSM |> 
    function 
    | None -> printfn "Invalid formula"
    | _    -> ()
    0 // return an integer exit code