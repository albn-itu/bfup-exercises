module Eval

open StateMonad

(* Code for testing *)

let hello =
    [ ('H', 4)
      ('E', 1)
      ('L', 1)
      ('L', 1)
      ('O', 1) ]

let state = mkState [ ("x", 5); ("y", 42) ] hello [ "_pos_"; "_result_" ]
let emptyState = mkState [] [] []

let arith (a: SM<int>) (b: SM<int>) (f: (int -> int -> int)) =
    a >>= (fun x -> b >>= (fun y -> ret (f x y)))

let add (a: SM<int>) (b: SM<int>) : SM<int> = arith a b (fun x y -> x + y)
let sub (a: SM<int>) (b: SM<int>) : SM<int> = arith a b (fun x y -> x - y)
let mul (a: SM<int>) (b: SM<int>) : SM<int> = arith a b (fun x y -> x * y)

let arith0Sens (a: SM<int>) (b: SM<int>) (f: (int -> int -> int)) =
    a
    >>= (fun x ->
        b
        >>= (fun y ->
            if y = 0 then
                fail DivisionByZero
            else
                ret (f x y)))

let div a b = arith0Sens a b (fun x y -> x / y)
let modulo a b = arith0Sens a b (fun x y -> x % y)

type aExp =
    | N of int
    | V of string
    | WL
    | PV of aExp
    | Add of aExp * aExp
    | Sub of aExp * aExp
    | Mul of aExp * aExp
    | Div of aExp * aExp
    | Mod of aExp * aExp
    | CharToInt of cExp

and cExp =
    | C of char (* Character value *)
    | CV of aExp (* Character lookup at word index *)
    | ToUpper of cExp
    | ToLower of cExp
    | IntToChar of aExp

type bExp =
    | TT (* true *)
    | FF (* false *)

    | AEq of aExp * aExp (* numeric equality *)
    | ALt of aExp * aExp (* numeric less than *)

    | Not of bExp (* boolean not *)
    | Conj of bExp * bExp (* boolean conjunction *)

    | IsVowel of cExp (* check for vowel *)
    | IsLetter of cExp (* check for letter *)
    | IsDigit of cExp (* check for digit *)

let (.+.) a b = Add(a, b)
let (.-.) a b = Sub(a, b)
let (.*.) a b = Mul(a, b)
let (./.) a b = Div(a, b)
let (.%.) a b = Mod(a, b)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj(b1, b2)

let (.||.) b1 b2 =
    ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)

let (.->.) b1 b2 =
    (~~b1) .||. b2 (* boolean implication *)

let (.=.) a b = AEq(a, b)
let (.<.) a b = ALt(a, b)
let (.<>.) a b = ~~(a .=. b)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b)

let (.>=.) a b =
    ~~(a .<. b) (* numeric greater than or equal to *)

let (.>.) a b =
    ~~(a .=. b)
    .&&. (a .>=. b) (* numeric greater than *)

let rec arithEval a : SM<int> =
    match a with
    | N n -> ret n
    | V v -> lookup v
    | WL -> wordLength
    | PV p -> arithEval p >>= (fun x -> pointValue x)
    | Add (a, b) -> add (arithEval a) (arithEval b)
    | Sub (a, b) -> sub (arithEval a) (arithEval b)
    | Mul (a, b) -> mul (arithEval a) (arithEval b)
    | Div (a, b) -> div (arithEval a) (arithEval b)
    | Mod (a, b) -> modulo (arithEval a) (arithEval b)
    | CharToInt (c) -> charEval c >>= (fun x -> ret (int x))

and charEval c : SM<char> =
    match c with
    | C c -> ret c
    | CV v -> arithEval v >>= (fun x -> characterValue x)
    | ToUpper c ->
        charEval c
        >>= (fun x -> ret (System.Char.ToUpper x))
    | ToLower c ->
        charEval c
        >>= (fun x -> ret (System.Char.ToLower x))
    | IntToChar i -> arithEval i >>= (fun x -> ret (char x))

let numericComparison (a: aExp) (b: aExp) (f: (int -> int -> bool)) : SM<bool> =
    arithEval a
    >>= (fun x -> arithEval b >>= (fun y -> ret (f x y)))

let isVowel (c: char) : bool =
    match System.Char.ToLower(c) with
    | 'a'
    | 'e'
    | 'i'
    | 'o'
    | 'u' -> true
    | _ -> false


let rec boolEval b : SM<bool> =
    match b with
    | TT -> ret true
    | FF -> ret false
    | AEq (a, b) -> numericComparison a b (fun x y -> x = y)
    | ALt (a, b) -> numericComparison a b (fun x y -> x < y)
    | Not b -> boolEval b >>= (fun x -> ret (not x))
    | Conj (b1, b2) ->
        boolEval b1
        >>= (fun x -> boolEval b2 >>= (fun y -> ret (x && y)))
    | IsVowel c -> charEval c >>= (fun x -> ret (isVowel x))

    | IsLetter c ->
        charEval c
        >>= (fun x -> ret (System.Char.IsLetter x))
    | IsDigit c ->
        charEval c
        >>= (fun x -> ret (System.Char.IsDigit x))


type stm =
    (* statements *)
    | Declare of string (* variable declaration *)
    | Ass of string * aExp (* variable assignment *)
    | Skip (* nop *)
    | Seq of stm * stm (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm (* while statement *)

let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

type StateBuilder() =

    member this.Bind(f, x) = f >>= x
    member this.Return(x) = ret x
    member this.ReturnFrom(x) = x
    member this.Delay(f) = f ()
    member this.Combine(a, b) = a >>= (fun _ -> b)

let prog = new StateBuilder()

let arithEval2 a = failwith "Not implemented"
let charEval2 c = failwith "Not implemented"
let rec boolEval2 b = failwith "Not implemented"

let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *)

type word = (char * int) list
type squareFun = word -> int -> int -> Result<int, Error>

let stmntToSquareFun stm = failwith "Not implemented"


type coord = int * int

type boardFun = coord -> Result<squareFun option, Error>

let stmntToBoardFun stm m = failwith "Not implemented"

type board =
    { center: coord
      defaultSquare: squareFun
      squares: boardFun }

let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
