module Exam.ThirdQuestion

(* Question 3.1 *)
let fib c = 
    let arr = Array.create (c + 1) 0UL

    let rec aux c : uint64 =
        match c with
        | 0 | 1 -> 1UL
        | n ->
            match arr.[c] with
            | 0UL ->
                arr.[c] <- (aux (c-1)) + (aux (c-2))
                arr.[c]
            | x -> x
    
    aux c

let calculateGoldenRatio n = 
    (float (fib (n+1))) / (float (fib n))

// Real solution
// Calculates fib while running, alot faster than my solution
let calculateGoldenRatioAlt x =
    let rec aux a b =
        function
        | 0 -> float b / float a
        | n -> aux b (a + b) (n - 1)

    aux 1 1 x


(* Question 3.2 *)

let grSeq = Seq.unfold (fun s -> Some (calculateGoldenRatio s, s + 1)) 0 

(* Question 3.3 *)

let rect x gr = gr * x * x
let goldenRectangleSeq x = Seq.map (rect x) grSeq

let height b gr = b * System.Math.Sqrt (float (gr * gr) - (1.0/4.0))
let triangle x gr = (x * height x gr) / 2.0
let goldenTriangleSeq x = Seq.map (triangle x) grSeq

(* Question 3.4 *)
let goldenRectangleTriangle x = 
    seq { for gr in grSeq do yield (rect x gr, triangle x gr) }

// Real solution
// Much better tbh
let goldenRectangleTriangleAlt b =
    let rec aux s1 s2 =
        seq {
            yield (Seq.head s1, Seq.head s2)
            yield! aux (Seq.tail s1) (Seq.tail s2)
        }

    aux (goldenRectangleSeq b) (goldenTriangleSeq b)
