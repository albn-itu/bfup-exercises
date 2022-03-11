module Assignment5

let sum (m: int) (n: int) : int =
    let rec sumA (acc: int) (m: int) (n: int) : int =
        match n with
        | 0 -> m + acc
        | _ -> sumA (m + n + acc) m (n - 1)

    sumA 0 m n

let length (xs: 'a list) : int =
    let rec lengthA (acc: int) (xs: 'a list) : int =
        match xs with
        | [] -> acc
        | _ :: ys -> lengthA (acc + 1) ys

    lengthA 0 xs

let foldBack (f: ('a -> 'b -> 'b)) (ls: 'a list) (acc: 'b) : 'b =
    let rec foldBackA (f: ('a -> 'b -> 'b)) (ls: 'a list) c : 'b =
        match ls with
        | [] -> c acc
        | x :: ys -> foldBackA f ys (fun y -> c (f x y))

    foldBackA f ls id

let factC (n: int) : int =
    let rec aux (n: int) c : int =
        match n with
        | 0 -> c 1
        | _ -> aux (n - 1) (fun x -> c (n * x))

    aux n id

let fibA (n: int) : int =
    let rec aux (c: int) (acc1: int) (acc2: int) : int =
        match c with
        | x when x = n -> acc1 + acc2
        | _ -> aux (c + 1) (acc2) (acc1 + acc2)

    match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> aux 2 0 1

let fibC (n: int) : int =
    let rec aux (x: int) (c: (int -> int)) : int =
        match x with
        | 0 -> c 0
        | 1 -> c 1
        | _ -> aux (x - 1) (fun y -> aux (x - 2) (fun z -> c (y + z)))

    aux n id
