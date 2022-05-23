module Exam.FirstQuestion

type Peano =
    | O
    | S of Peano

let rec toInt = function
    | O -> 0u
    | S y -> 1u + toInt y

let rec fromInt = function
    | 0u -> O
    | n -> S (fromInt (n - 1u))

let rec add a b =
    match a with
    | O -> b
    | S x -> S (add x b)

let rec mult a b =
    match a with
    | O -> O
    | S x -> add b (mult x b)

let rec pow a b =
    match b with
    | O -> S O
    | S x -> mult a (pow a x)

let tailAdd a b =
    let rec aux a acc =
        match a with
        | O -> acc
        | S x -> aux x (S acc)

    aux a b

let tailMult a b =
    let rec aux a acc =
        match a with
        | O -> acc
        | S x -> aux x (tailAdd b acc)
    
    aux a O

let tailPow a b =
    let rec aux b acc =
        match b with
        | O -> acc
        | S x -> aux x (tailMult a acc)
    
    aux b (S O)

let rec loop (f: 'a -> 'a) (acc: 'a) (p: Peano) : 'a =
    match p with
    | O -> acc
    | S x -> loop f (f acc) x

let loopAdd a b =
    loop S a b

let loopMult a b =
    loop (loopAdd b) O a

let loopPow a b =
    loop (loopMult a) (S O) b