module Exam

(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but
   it does allow you to work in interactive mode and you can just remove the '='
   to make the project compile work again.

   Do not remove the line (even though that does work) because you may inadvertantly
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode.

   Alternative, keep the line as is, but load ExamInteractive.fsx into the interactive environment
   *)
(* module Exam2020_2 = *)

(* 1: Binary search trees *)

type 'a bintree =
    | Leaf
    | Node of 'a bintree * 'a * 'a bintree

(* Question 1.1 *)

let rec insert x = 
    function
    | Leaf -> Node (Leaf, x, Leaf)
    | Node (t1, y, t2) when x <= y -> Node (insert x t1, y, t2)
    | Node (t1, y, t2) -> Node(t1, y, insert x t2)

(* Question 1.2 *)
let fromList lst =
    let rec aux acc =
        function
        | [] -> acc
        | x :: lst -> aux (insert x acc) lst

    aux Leaf lst


(* Question 1.3 *)
// inorder traversal
let rec fold f acc =
    function
    | Leaf -> acc
    | Node (left, x, right) -> 
        let acc = fold f acc left
        fold f (f acc x) right

// reverse inorder traversal
let rec foldBack f acc =
    function
    | Leaf -> acc
    | Node (left, x, right) -> 
        let acc = foldBack f acc right
        foldBack f (f acc x) left

// We use foldback so we don't have to reverse the list
let inOrder t = foldBack (fun acc x -> x::acc) [] t

(* Question 1.4 *)

(*

    Q: Consider the following map function

    *)

let rec badMap f =
    function
    | Leaf -> Leaf
    | Node (l, y, r) -> Node(badMap f l, f y, badMap f r)

(*
    Even though the type of this function is `('a -> 'b) -> 'a bintree -> 'b bintree`
    as we would expect from a map function, this  function does not do what
    we want it to do. What is the problem? Provide an example to demonstrate the problem.

    A: It does not ensure tree correctness. Example the tree
        10
    3       20
    and the function (fun x -> x % 2) would produce
        0
    1       0
    Which by definition is incorrect
    *)

let rec map f t = fold (fun acc x -> insert (f x) acc) Leaf t 

(* 2: Code Comprehension *)
let rec foo =
    function
    | [ x ] -> [ x ]
    | x :: y :: xs when x > y -> y :: (foo (x :: xs))
    | x :: xs -> x :: foo xs

let rec bar =
    function
    | [ x ] -> true
    | x :: y :: xs -> x <= y && bar (y :: xs)

let rec baz =
    function
    | [] -> []
    | lst when bar lst -> lst
    | lst -> baz (foo lst)


(* Question 2.1 *)

(*

    Q: What are the types of functions foo,  bar, and baz?

    A:  foo has type 'a list -> 'a list when 'a: comparison
        bar has type 'a list -> bool when 'a: comparison
        baz has type 'a list -> 'a list when 'a:comparison


    Q: What do functions ```bar```, and ```baz``` do
       (not `foo`, we admit that it is a bit contrived)?
       Focus on what they do rather than how they do it.

    A:  bar returns true if the list is sorted otherwise false
        baz checks if the list is sorted, if it isnt it sorts it, if is it returns it

    Q: What would be appropriate names for functions
       foo, bar, and baz?

    A: 
       
       foo = sortPass, it's a specific algorithm and only one pass
       bar = isSorted
       baz = sort

    *)


(* Question 2.2 *)


(*
    The functions foo and bar generate a warning during compilation:
    'Warning: Incomplete pattern matches on this expression.'

    Q: Why does this happen, and where?

    A:
        In both it happens because there is no case for an empty list [].
        Match expressions has to be exhaustive

    Q: For these particular three functions will this incomplete
       pattern match ever cause problems for any possible execution of baz?
       If yes, why; if no, why not.

    A: No, baz actually handles this case, and therefore does not fail. But foo [];; will fail

    *)

let foo2 = 
    function
    | [] -> []
    | [ x ] -> [ x ]
    | x :: y :: xs when x > y -> y :: (foo (x :: xs))
    | x :: xs -> x :: foo xs

let bar2 =
    function
    | [] | [ _ ] -> true
    | x :: y :: xs -> x <= y && bar (y :: xs)

(* Uncomment code to run after you have written foo2 and bar2 *)

let rec baz2 =
    function
    | lst when bar2 lst -> lst
    | lst               -> baz2 (foo2 lst)

(* Question 2.3 *)

(* Consider this alternative definition of *)

let rec foo3 =
    function
    | [ x ] -> [ x ]
    | x :: xs -> x :: foo3 xs
    | x :: y :: xs when x > y -> y :: (foo3 (x :: xs))

(*

    Q: Do the functions `foo` and `foo3` produce the same output for all possible inputs?
       If yes, why; if no why not and provide a counter example.

    A: This function does nothing, the last case is never matched, as F# takes the first case that matches
        foo [2;1] = [1;2]
        foo3 [2;1] = [2;1]

    *)

(* Question 2.4 *)

let bar3 lst = 
    List.pairwise lst 
    |> List.fold (fun st (x, y) -> if st && x > y then false else st) true

(* Question 2.5 *)

(*

    Q: The function foo or baz is not tail recursive. Which one and why?

    A: foo is not tail recursive, let's take foo [1;2;4;3] as an example
    foo [1;2;4;3]
    1 :: (foo [2;4;3])
    1 :: (2 :: (foo [4;3]))
    1 :: (2 :: (3 :: (foo [4])))
    1 :: (2 :: (3 :: ([4])))
    1 :: (2 :: ([3;4]))
    1 :: [2;3;4]
    [1;2;3;4]
    
    As we can see foo depends on the execution of it's own recursive call to finish before it can add an element to the resulting list

    *)

(* ONLY implement the one that is NOT already tail recursive *)

let fooTail lst =
    let rec aux con =
        function
        | [] -> con []
        | [ x ] -> con [ x ]
        | x :: y :: xs when x > y -> aux (fun l -> con (y::l)) (x::xs)
        | x :: xs -> aux (fun l -> con (x::l)) xs

    aux id lst


let bazTail _ = failwith "not implemented"

(* 3: Big Integers *)

(* Question 3.1 *)


type bigInt = BI of int list

let charToInt (c: char) = int c - int '0'

let fromString str = 
    BI(List.ofSeq str |> List.rev |> List.map charToInt)

let toString (BI(lst)) = 
    List.rev lst |> List.map (fun x -> x.ToString()) |> String.concat ""

(* Question 3.2 *)

let add (BI(x)) (BI(y)) =
    let calc x y prev = 
        let rem = (x+y+prev) % 10
        (rem, (x+y+prev)/10)

    let rec aux acc fromPrev =
        function
        | ([], []) when fromPrev <> 0 -> fromPrev::acc
        | ([], []) -> acc
        | (x::xs, []) -> 
            let cur, next = calc x 0 fromPrev
            aux (cur :: acc) next (xs, [])
        | ([], y::ys) ->
            let cur, next = calc 0 y fromPrev
            aux (cur :: acc) next ([], ys)
        | (x::xs, y::ys) ->
            let cur, next = calc x y fromPrev
            aux (cur :: acc) next (xs, ys)

    BI(aux [] 0 (x,y) |> List.rev)

(* Question 3.3 *)

let actualMult x y =
    let calc x y prev = 
        let rem = (x*y+prev) % 10
        (rem, (x*y+prev)/10)

    let rec aux acc fromPrev =
        function
        | [] when fromPrev <> 0 && fromPrev > 10 -> 
            let cur, next = calc 0 0 fromPrev
            aux (cur :: acc) next []
        | [] when fromPrev <> 0 -> fromPrev::acc
        | [] -> acc
        | x::xs -> 
            let cur, next = calc x y fromPrev
            aux (cur :: acc) next xs

    match y with
    | 0 -> [0]
    | _ -> aux [] 0 x |> List.rev

let multSingle (BI(x)) y =
    BI(actualMult x y)

(* Question 3.4 *)

let mult (BI(x)) (BI(y)) =
    let rec aux acc num = 
        function
        | [] -> acc |> List.fold (add) (BI([0]))    
        | y::ys -> 
            let res = actualMult x y
            let zeroes = [for _ in 1u..num -> 0]

            aux (BI(zeroes @ res)::acc) (num+1u) ys
    
    aux ([]: bigInt list) 0u y

(* Question 3.5 *)

let fact x numThreads =
    let calc (lst: int list) = 
        async {
            return lst |> List.fold (fun st x -> x.ToString() |> fromString |> mult st) (BI([1]))
        }

    match x with
    | 0 -> BI([1])
    | _ ->
        [for i in 1..x -> i]
        |> List.splitInto numThreads
        |> List.map calc
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.fold (fun st l -> mult l st) (BI([1]))

(* 4: Lazy lists *)

type 'a llist = Cons of (unit -> ('a * 'a llist))

let rec llzero = Cons(fun () -> (0, llzero))

(* Question 4.1 *)

let step (Cons(f)) =
    f ()

let cons x ll =
    Cons (fun () -> (x, ll))

(* Question 4.2 *)

let init f =
    let rec aux x = (fun () -> (f x, Cons(aux (x+1))))

    Cons(aux 0)

(* Question 4.3 *)

let rec llmap (f: 'a -> 'b) ll : 'b llist=
    let (h1, tl) = step ll
    Cons(fun () -> (f h1, llmap f tl))

(* Question 4.4 *)

let rec filter f ll =
    match step ll with
    | (h1, tl) when f h1 -> Cons(fun () -> (h1, filter f tl))
    | (_, tl) -> filter f tl 

(* Question 4.5 *)

let takeFirst x ll =
    let rec aux c ll = 
        function
        | 0 -> c ([], ll)
        | n -> 
            let (h, ll) = step ll
            aux (fun (lst, ll) -> c (h::lst, ll)) ll (n-1)
    
    aux id ll x 

(* Question 4.6 *)

let rec unfold generator st =
    Cons(fun () -> 
        let (x, st') = generator st
        (x, unfold generator st')
    )

(* Consider the following two implementations of Fibonacci sequences fibll1 and fibll2: *)

let fib x =
    let rec aux acc1 acc2 =
        function
        | 0 -> acc1
        | x -> aux acc2 (acc1 + acc2) (x - 1)

    aux 0 1 x

(* Uncomment after you have implemented init and unfold *)

(*
    let fibll1 = init fib
    let fibll2 = unfold (fun (acc1, acc2) -> (acc1, (acc2, acc1 + acc2))) (0, 1)
  *)
(*

    Q: Both fibll1 and fibll2 correctly calculate a lazy list of Fibonacci numbers.
       Which of these two lazy lists is the most efficient implementation and why?

    A: that would be fibll2. fibll1 would calculate fib(n-1) and fib(n-2) for every n while fibll2 will reuse the old results thereby skipping execution

    *)
