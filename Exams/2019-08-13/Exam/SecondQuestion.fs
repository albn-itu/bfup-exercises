module Exam.SecondQuestion

// Original code:
let f s =
    let l = String.length s
    let rec aux =
        function
        | i when i = l -> []
        | i -> s.[i] :: aux (i + 1)

    aux 0

let g s = 
    s |> f |>
    List.filter System.Char.IsLetter |>
    List.map System.Char.ToLower |>
    fun lst -> lst = List.rev lst

// Question 2.1 - What are the types
// f: string -> char list
// g: string -> bool

// Question 2.1 - What do they do?
// f converts a string into a list of it's chars
// g checks if the list is the same forwards and backwards 

// Question 2.1 - What would be appropriate names
// f: toCharList
// g: isPalindrome 

// Question 2.2
// Create a function f2 that behaves the same as f but uses list comprehension
let f2 (s: string) = [ for i in 0..(String.length s - 1) do yield s.[i] ]
// Better solution:
let f2Alt s = [ for c in s do yield c ]

// Question 2.3
// Write g using >> instead of |>
let g2 =
    f2 
    >> List.filter System.Char.IsLetter 
    >> List.map System.Char.ToLower
    >> fun lst -> lst = List.rev lst

// Question 2.4 - Why is it not tail recursive?
// Let's look at a call
// f "Hello"
// l = 5
// aux 0
// s.[0] :: aux(0+1)
// s.[0] :: (s.[1] :: aux(1+1))
// s.[0] :: (s.[1] :: (s.[2] :: aux(2+1)))
// s.[0] :: (s.[1] :: (s.[2] :: (s.[3] :: aux(3+1))))
// s.[0] :: (s.[1] :: (s.[2] :: (s.[3] :: (s.[4] :: aux(4+1)))))
// s.[0] :: (s.[1] :: (s.[2] :: (s.[3] :: (s.[4] :: [])))) // endpoint of the recursion
// s.[0] :: (s.[1] :: (s.[2] :: (s.[3] :: ["o"])))
// s.[0] :: (s.[1] :: (s.[2] :: ["l"; "o"]))
// s.[0] :: (s.[1] :: ["l'; "l"; "o"])
// s.[0] :: ["e"; "l'; "l"; "o"]
// ["h"; "e"; "l'; "l"; "o"]

// This evaluation shows, that we cannot do the last operation of the function call before the recursive call is done.
// This creates a huge call stack, and is non-tail recursive

let fTail s =
    let l = String.length s
    let rec aux acc =
        function
        | -1 -> acc
        | i -> aux (s.[i] :: acc) (i-1)

    aux [] (l-1)

// Continuations
let fTailCon s =
    let l = String.length s
    let rec aux c =
        function
        | i when i = l -> c []
        | i -> aux (fun ls -> c (s.[i] :: ls)) (i + 1)

    aux id 0

// For each char
//      Check if its a letter
//      Lowercase it if it is
//      Add back into list, in reverse
let lAt (s: string) i = System.Char.IsLetter s.[i] 

let gOpt (s: string) : bool =
    let rec aux left right =
        match left > right with
        | true -> true
        | false -> 
            if not (lAt s left) then
                aux (left + 1) right
            else if not (lAt s right) then
                aux left (right - 1)
            else
                if (System.Char.ToLower s.[left]) = (System.Char.ToLower s.[right]) then
                    aux (left + 1) (right - 1)
                else
                    false
    
    aux 0 (String.length s - 1)

// Better solution
let gOptAlt (s: string) =
    let rec aux left =
        function
        | right when left >= right -> true
        | right when not (System.Char.IsLetter s.[left]) -> aux (left + 1) right
        | right when not (System.Char.IsLetter s.[right]) -> aux left (right - 1)
        | right when (System.Char.ToLower s.[left]) = (System.Char.ToLower s.[right]) -> aux (left + 1) (right - 1)
        | _ -> false

    aux 0 (String.length s - 1)