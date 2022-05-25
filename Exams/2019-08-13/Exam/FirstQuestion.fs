module Exam.FirstQuestion

type Sum<'A, 'B> =
| Left of 'A
| Right of 'B

(* Question 1.1 *)

let sum1 : Sum<int list, bool option> = Left [1] 
let sum2 : Sum<int list, bool option> = Right (Some true)

let sumMap f g s =
    match s with
    | Left x -> f x
    | Right y -> g y

(* Question 1.2 *)

type SumColl<'A, 'B> =
| Nil
| CLeft of 'A * SumColl<'A, 'B>
| CRight of 'B * SumColl<'A, 'B>

let sumColl = CRight (0, CLeft ([true], Nil))

let rec ofList l =
    match l with
    | elem::l' -> 
        match elem with
        | Left x -> CLeft (x, ofList l')
        | Right y -> CRight (y, ofList l')
    | [] -> Nil

// Much better solution
let rec ofListAlt l =
    match l with
    | Left x::l' -> CLeft (x, ofList l')
    | Right y::l' -> CRight (y, ofList l')
    | [] -> Nil


(* Question 1.3 *)

let reverse col = 
    let rec aux col acc = 
        match col with
        | CLeft (a, col') -> aux col' (CLeft (a, acc))
        | CRight (a, col') -> aux col' (CRight (a, acc))
        | Nil -> acc

    aux col Nil

(* Question 1.4 *)

let ofList2 l = 
    let aux t state =
        match t with
        | Left x -> CLeft (x, state)
        | Right y -> CRight (y, state)

    List.foldBack (aux) l Nil 

// Much better solutin
let lcon s a = CLeft (a, s)
let rcon s b = CRight (b, s)

let ofList2Alt l =
    List.foldBack (fun y s -> sumMap (lcon s) (rcon s) y) l Nil

(* Question 1.5 *)

let foldBackSumColl f g col x : 'c = 
    let rec aux col : 'c =
        match col with
        | CLeft (a, col') -> f a (aux col')
        | CRight (b, col') -> g b (aux col')
        | Nil -> x

    aux col