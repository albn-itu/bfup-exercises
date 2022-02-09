namespace Assignment1

module Assignment1 =
  let sqr (x: int) : int = x * x

  let pow (x: float) (n: float) : float = x ** n

  let rec sum (n: int) : int =
    match n with
    | 0 -> 0
    | _ -> n + sum(n-1)
   
  let rec fib (n: int) : int =
    match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> fib(n-1) + fib(n-2)

  let dup (s: string) : string =
    s + s

  let dupn (s: string) (n: int) : string =
    let mutable out = ""
    for _ in 1..n do
      out <- out + s
    out

  let rec bin (n: int, k:int) : int =
    match (n, k) with
    | (n,k) when n = k -> 1
    | (n,k) when k = 0 -> 1
    | _ -> bin(n-1, k-1) + bin(n-1, k)

  let timediff (t1hh: int, t1mm: int) (t2hh: int, t2mm: int) : int =
    ((t2hh - t1hh) * 60) + (t2mm - t1mm)

  let minutes (hh: int, mm: int) : int =
    timediff (0, 0) (hh, mm)

  let curry (f: ('a * 'b -> 'c)) (x: 'a) (y: 'b) : 'c =
    f(x, y)

  let uncurry (f: ('a -> 'b -> 'c)) (x: 'a, y: 'b) : 'c =
    f(x)(y)

  let empty (def: char * int) : (int -> char * int) =
    fun(i) -> def

  let add (newPos: int) (cv: char * int) (word: (int -> char * int)) : (int -> char * int) =
    fun(i) ->
      match i with
      | i when i = newPos -> cv
      | _ -> word(i)

  let hello (pos: int) : char * int =
    let funct = 
      empty (char 0, 0)
      |> add 4 ('O', 1)
      |> add 3 ('L', 1) 
      |> add 2 ('L', 1)
      |> add 1 ('E', 1)
      |> add 0 ('H', 4)

    funct pos

  let letterScore (word: int -> char * int) (pos: int) (scoreMultiplier: int) : int =
    let (_, score) = word pos

    score * scoreMultiplier

  let singleLetterScore (word: int -> char * int) (pos: int) : int =
    letterScore word pos 1

  let doubleLetterScore (word: int -> char * int) (pos: int) : int =
    letterScore word pos 2

  let tripleLetterScore (word: int -> char * int) (pos: int) : int =
    letterScore word pos 3