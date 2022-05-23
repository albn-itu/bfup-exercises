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
// It happens in the function expression, because F# doesn't consider guards, even though they are exhaustive. We have also not handled the case where there is only 1 element in the list.

// Write a function f2 that does the same thing as f and that does not have this problem.
let rec f x =
    function
    | [] -> None
    | [ y ] ->
        match y with
        | y when y == x -> Some []
        | _ -> None
    | y :: ys ->
        match y with
        | y when y == x -> Some ys
        | _ ->
            match f x ys with
            | Some ys' -> Some(y :: ys')
            | None -> None
