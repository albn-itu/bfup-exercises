module Exam.SecondQuestion

// Original code is pasted below:
// let rec f x =
//     function
//     | []                -> None
//     | y::ys when x = y  -> Some ys
//     | y::ys when x <> y ->
//         match f x ys with
//         | Some ys' -> Some (y::ys')
//         | None     -> None

// let rec g xs =
//     function
//     | []    -> xs = []
//     | y::ys ->
//         match f y xs with
//         | Some xs' -> g xs' ys
//         | None     -> false

// Question 2.1
// What are the types of functions f and g?
// f: 'a -> list<'a> -> option<list<'a>>
// g: list<'a> -> list<'a> -> bool

// What do functions f and g do?
// f removes the given element from the list, at the first instance it is found.
// g checks if 2 lists are equal

// What would be appropriate names for f and g?
// f: removeFirst
// g: equal

// Question 2.2
// The function f generates a warning during compilation: warning FS0025: Incomplete pattern matches on this expression..
// Why does this happen, and where?
// It happens in the function expression, because F# doesn't consider guards, even though they are exhaustive. Therefore, since we have already handled all cases, we can change one of the guards to be a wildcard 

// Write a function f2 that does the same thing as f and that does not have this problem.
let rec f2 x =
    function
    | []                -> None
    | y::ys when x = y  -> Some ys
    | y::ys ->
        match f2 x ys with
        | Some ys' -> Some (y::ys')
        | None     -> None

// Question 2.3
// Replace match option with option.map
let rec fOpt x =
    function
    | []                -> None
    | y::ys when x = y  -> Some ys
    | y::ys ->
        (fOpt x ys)
        |> Option.map (fun ys' -> y::ys')

let rec gOpt xs =
    function
    | []    -> xs = []
    | y::ys ->
        (fOpt y xs)
        |> Option.map (fun xs' -> gOpt xs' ys)
        |> Option.defaultValue false

// Question 2.4
// Given a list l = [1;2;3;2], and x = 2, f evaluates to:
// f 2 [1;2;3;2]
//   f 2 1::[2;3;2]
// When f travels up the call stack again, if it found the element, it then has to recreate the list that it had just destroyed.
// Therefore we shall rewrite f as fTail.

// For good measure here is g, which as it simply returns a boolean, doesnt suffer from the same problem
// Given a list l = [1;2;3] and x = [3;2;1], g evalues to
// g [1;2;3] [3;2;1] -> 3::[2;1] -> f 3 [1;2;3] -> [1;2]
//  g [1;2] [2;1] -> 2::[1] -> f 2 [1;2] -> [1]
//    g [1] [1] -> 1::[] -> f 1 [1] -> []
//      g [] [] -> true

let fTail x ys =
    let rec aux ys c =
        match ys with
        | [] -> None
        | y::ys' when x = y -> Some (c ys')
        | y::ys' -> aux ys' (fun ys'' -> c (y::ys''))

    aux ys id
