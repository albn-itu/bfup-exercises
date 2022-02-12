namespace Assignment2

module Assignment2 =
    let rec downto1 (n: int) : int list =
        if n <= 0 then
            []
        else
            n :: downto1 (n - 1)

    let rec downto2 (n: int) : int list =
        match n with
        | n when n <= 0 -> []
        | _ -> n :: downto2 (n - 1)

    let rec removeOddIdx (xs: 'a list) : 'a list =
        match xs with
        | x :: _ :: xs -> x :: removeOddIdx (xs)
        | _ -> xs // Must be 1 or less elements left

    let rec combinePair (xs: 'a list) : ('a * 'a) list =
        match xs with
        | x :: y :: xs -> (x, y) :: combinePair (xs)
        | _ -> []

    type complex = float * float

    let mkComplex (x: float) (y: float) : complex = (x, y)

    let complexToPair (c: complex) : (float * float) = c

    let (|+|) (x: complex) (y: complex) : complex =
        let (a, b) = complexToPair x
        let (c, d) = complexToPair y

        mkComplex (a + c) (b + d)

    let (|*|) (x: complex) (y: complex) : complex =
        let (a, b) = complexToPair x
        let (c, d) = complexToPair y

        mkComplex (a * c - b * d) (b * c + a * d)

    let (|-|) (x: complex) (y: complex) : complex =
        let (c, d) = complexToPair y

        x |+| mkComplex (-c) (-d)

    let (|/|) (x: complex) (y: complex) : complex =
        let (c, d) = complexToPair y

        let denominator = (c ** 2.0 + d ** 2.0)

        x
        |*| mkComplex (c / denominator) (-d / denominator)

    let explode1 (s: string) : char list = s.ToCharArray() |> List.ofArray

    let rec explode2 (s: string) : char list =
        match s with
        | "" -> []
        | _ -> s.[0] :: explode2 (s.Remove(0, 1))

    let implode (cs: char list) : string =
        List.foldBack (fun (c: char) (s: string) -> c.ToString() + s) cs ""

    let implodeRev (cs: char list) : string =
        List.fold (fun (s: string) (c: char) -> c.ToString() + s) "" cs

    // let toUpper (s: string) : string =
    // explode1 s |> List.map (fun c -> System.Char.ToUpper(c)) |> implode

    let toUpper =
        explode1
        >> (List.map System.Char.ToUpper)
        >> implode

    exception InvalidAckArgument of string

    let rec ack (mn: int * int) : int =
        match mn with
        | (m, n) when m = 0 -> n + 1
        | (m, n) when m > 0 && n = 0 -> ack (m - 1, 1)
        | (m, n) when m > 0 && n > 0 -> ack (m - 1, ack (m, n - 1))
        | _ -> raise (InvalidAckArgument "Invalid Ack argument")

    let time (f: unit -> 'a) : 'a * System.TimeSpan =
        let start = System.DateTime.Now
        let result = f ()
        let finish = System.DateTime.Now
        (result, finish - start)

    let timeArg1 (f: 'a -> 'b) (arg: 'a) : 'b * System.TimeSpan = time (fun () -> f arg)

    let rec downto3 (f: int -> 'a -> 'a) (n: int) (e: 'a) : 'a =
        match n with
        | n when n > 0 -> downto3 f (n - 1) (f n e)
        | _ -> e // Equivalent to <= e

    let fac (n: int) : int = downto3 (fun (n: int) (x: int) -> n * x) n 1

    let range (g: int -> 'a) (n: int) : 'a list =
        match n with
        | n when n > 0 -> downto3 (fun (n: int) (x: 'a list) -> g n :: x) n []
        | _ -> []

    type word = (char * int) list

    let hello: word =
        [ ('H', 4)
          ('E', 1)
          ('L', 1)
          ('L', 1)
          ('O', 1) ]

    type squareFun = word -> int -> int -> int

    let xLetterScore (w: word) (pos: int) (acc: int) (x: int) : int =
        let (square, score) = w.[pos]
        score * x + acc

    let singleLetterScore: squareFun =
        fun (w: word) (pos: int) (acc: int) -> xLetterScore w pos acc 1

    let doubleLetterScore: squareFun =
        fun (w: word) (pos: int) (acc: int) -> xLetterScore w pos acc 2

    let tripleLetterScore: squareFun =
        fun (w: word) (pos: int) (acc: int) -> xLetterScore w pos acc 3

    let xWordScore (acc: int) (x: int) = acc * x

    let doubleWordScore: squareFun =
        fun (w: word) (pos: int) (acc: int) -> xWordScore acc 2

    let tripleWordScore: squareFun =
        fun (w: word) (pos: int) (acc: int) -> xWordScore acc 3

    let isVowel (c: char) : bool =
        match System.Char.ToLower(c) with
        | 'a'
        | 'e'
        | 'i'
        | 'o'
        | 'u' -> true
        | _ -> false

    let getConsonants (w: word) : char list =
        w
        |> List.map fst 
        |> List.filter (fun (c) -> not (isVowel c))

    let oddConsonants: squareFun =
        fun (w: word) (pos: int) (acc: int) ->
            match (getConsonants w).Length with
            | x when x % 2 = 0 -> acc
            | _ -> -acc

    type square = (int * squareFun) list

    let SLS: square = [ (0, singleLetterScore) ]
    let DLS: square = [ (0, doubleLetterScore) ]
    let TLS: square = [ (0, tripleLetterScore) ]
    let DWS: square = SLS @ [ (1, doubleWordScore) ]
    let TWS: square = SLS @ [ (1, tripleWordScore) ]

    let partiallyApplyScore (word: word) (pos: int) (sq: square) : (int * (int -> int)) list =
        sq
        |> List.map (fun (score: int, f: squareFun) -> (score, (fun (acc: int) -> f word pos acc)))

    let calculatePoints (squares: square list) (w: word) : int =
        squares
        |> List.mapi (fun (pos: int)  (sq: square) -> partiallyApplyScore w pos sq)
        |> List.fold (fun st sq -> sq @ st) []
        |> List.sortBy fst 
        |> List.map snd
        |> List.fold (fun (f: int -> int) (g: int -> int) -> f >> g) id 
        <| 0
