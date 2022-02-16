namespace Assignment3

module Assignment3 =
    type word = (char * int) list

    type squareFun = word -> int -> int -> int

    let hello: word =
        [ ('H', 4)
          ('E', 1)
          ('L', 1)
          ('L', 1)
          ('O', 1) ]


    type aExp =
        | N of int // Integer value
        | V of string // Variable
        | WL // Length of the word
        | PV of aExp // Point value of character at specific word index
        | Add of aExp * aExp // Addition
        | Sub of aExp * aExp // Subtraction
        | Mul of aExp * aExp // Multiplication
        | Mod of aExp * aExp // Modulo

    let (.+.) a b = Add(a, b)

    let (.-.) a b = Sub(a, b)

    let (.*.) a b = Mul(a, b)

    let (.%.) a b = Mod(a, b)

    exception ArithmeticError of string

    let rec arithEvalSimple (a: aExp) : int =
        match a with
        | N n -> n
        | V _ -> raise (ArithmeticError "Variable not supported")
        | WL -> raise (ArithmeticError "Variable not supported")
        | PV _ -> raise (ArithmeticError "Variable not supported")
        | Add (a, b) -> arithEvalSimple a + arithEvalSimple b
        | Sub (a, b) -> arithEvalSimple a - arithEvalSimple b
        | Mul (a, b) -> arithEvalSimple a * arithEvalSimple b
        | Mod (a, b) -> arithEvalSimple a % arithEvalSimple b

    let findWithDefault (k: string) (m: Map<string, int>) : int =
        match Map.tryFind k m with
        | Some v -> v
        | None -> 0

    let rec arithEvalState (a: aExp) (s: Map<string, int>) : int =
        let eval x = arithEvalState x s

        match a with
        | N n -> n
        | V v -> findWithDefault v s
        | WL -> raise (ArithmeticError "Variable not supported")
        | PV _ -> raise (ArithmeticError "Variable not supported")
        | Sub (a, b) -> eval a - eval b
        | Mul (a, b) -> eval a * eval b
        | Add (a, b) -> eval a + eval b
        | Mod (a, b) -> eval a % eval b

    let arithSingleLetterScore = PV(V "_pos_") .+. (V "_acc_")

    let arithDoubleLetterScore =
        ((N 2) .*. PV(V "_pos_")) .+. (V "_acc_")

    let arithTripleLetterScore =
        ((N 3) .*. PV(V "_pos_")) .+. (V "_acc_")

    let arithDoubleWordScore = N 2 .*. V "_acc_"

    let arithTripleWordScore = N 3 .*. V "_acc_"

    let rec arithEval (a: aExp) (w: word) (s: Map<string, int>) : int =
        let eval x = arithEval x w s

        match a with
        | N n -> n
        | V v -> findWithDefault v s
        | WL -> w.Length
        | PV pv -> w.[eval pv] |> snd
        | Sub (a, b) -> eval a - eval b
        | Mul (a, b) -> eval a * eval b
        | Add (a, b) -> eval a + eval b
        | Mod (a, b) -> eval a % eval b

    type cExp =
        | C of char
        | ToUpper of cExp
        | ToLower of cExp
        | CV of aExp

    let rec charEval (c: cExp) (w: word) (s: Map<string, int>) : char =
        let eval x = charEval x w s

        match c with
        | C c -> c
        | ToUpper c -> eval c |> System.Char.ToUpper
        | ToLower c -> eval c |> System.Char.ToLower
        | CV c -> w.[arithEval c w s] |> fst

    let isVowel (c: char) : bool =
        match System.Char.ToLower(c) with
        | 'a'
        | 'e'
        | 'i'
        | 'o'
        | 'u' -> true
        | _ -> false

    type bExp =
        | TT (* true *)
        | FF (* false *)

        | AEq of aExp * aExp (* numeric equality *)
        | ALt of aExp * aExp (* numeric less than *)

        | Not of bExp (* boolean not *)
        | Conj of bExp * bExp (* boolean conjunction *)

        | IsDigit of cExp (* check for digit *)
        | IsLetter of cExp (* check for letter *)
        | IsVowel of cExp (* check for vowel *)

    let (~~) b = Not b

    let (.&&.) b1 b2 = Conj(b1, b2)

    let (.||.) b1 b2 =
        ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)

    let (.=.) a b = AEq(a, b)

    let (.<.) a b = ALt(a, b)

    let (.<>.) a b = ~~(a .=. b) (* numeric inequality *)

    let (.<=.) a b =
        a .<. b
        .||. ~~(a .<>. b) (* numeric less than or equal to *)

    let (.>=.) a b =
        ~~(a .<. b) (* numeric greater than or equal to *)

    let (.>.) a b =
        ~~(a .=. b)
        .&&. (a .>=. b) (* numeric greater than *)

    let rec boolEval (b: bExp) (w: word) (s: Map<string, int>) : bool =
        let eval b x = b x w s
        let aEval x = eval arithEval x
        let bEval x = eval boolEval x
        let cEval x = eval charEval x

        match b with
        | TT -> true
        | FF -> false

        | AEq (a, b) -> aEval a = aEval b
        | ALt (a, b) -> aEval a < aEval b

        | Not b -> bEval b |> not
        | Conj (b1, b2) -> bEval b1 && bEval b2

        | IsDigit c -> cEval c |> System.Char.IsDigit
        | IsLetter c -> cEval c |> System.Char.IsLetter
        | IsVowel c -> cEval c |> isVowel

    let isConsonant (c: cExp) : bExp = ~~(IsVowel c)

    type stmnt =
        | Skip (* does nothing *)
        | Ass of string * aExp (* variable assignment *)
        | Seq of stmnt * stmnt (* sequential composition *)
        | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
        | While of bExp * stmnt (* while statement *)

    let rec evalStmnt (stm: stmnt) (w: word) (s: Map<string, int>) : Map<string, int> =
        let eval x ss = evalStmnt x w ss

        match stm with
        | Skip -> s
        | Ass (x, a) -> Map.add x (arithEval a w s) s
        | Seq (stm1, stm2) -> eval stm1 s |> eval stm2
        | ITE (guard, stm1, stm2) ->
            if (boolEval guard w s) then
                eval stm1 s
            else
                eval stm2 s
        | While (guard, stm) ->
            if (boolEval guard w s) then
                eval stm s |> eval (While(guard, stm))
            else
                s

    let stmntToSquareFun (stm: stmnt) : squareFun =
        fun (w: word) (pos: int) (acc: int) ->
            let s =
                Map.ofList [ ("_pos_", pos)
                             ("_acc_", acc) ]

            evalStmnt stm w s |> Map.find "_result_"

    let singleLetterScore =
        stmntToSquareFun (Ass("_result_", arithSingleLetterScore))

    let doubleLetterScore =
        stmntToSquareFun (Ass("_result_", arithDoubleLetterScore))

    let tripleLetterScore =
        stmntToSquareFun (Ass("_result_", arithTripleLetterScore))

    let doubleWordScore =
        stmntToSquareFun (Ass("_result_", arithDoubleWordScore))

    let tripleWordScore =
        stmntToSquareFun (Ass("_result_", arithTripleWordScore))

    let containsNumbers =
        stmntToSquareFun (
            Seq(
                Ass("_result_", V "_acc_"),
                While(
                    V "i" .<. WL,
                    ITE(
                        IsDigit(CV(V "i")),
                        Seq(Ass("_result_", V "_result_" .*. N -1), Ass("i", WL)),
                        Ass("i", V "i" .+. N 1)
                    )
                )
            )
        )

    let oddConsonants =
        Seq(
            Seq(Ass("_result_", V "_acc_"), Ass("_acc_", N 0)),
            Seq(
                While(
                    V "i" .<. WL,
                    ITE(
                        IsVowel(CV(V "i")),
                        Ass("i", V "i" .+. N 1),
                        Seq(Ass("_acc_", V "_acc_" .+. N 1), Ass("i", V "i" .+. N 1))
                    )
                ),
                ITE(V "_acc_" .%. N 2 .=. N 0, Skip, Ass("_result_", V "_result_" .*. N -1))
            )
        )

    type square = (int * squareFun) list

    type square2 = (int * stmnt) list

    let SLS =
        [ (0, Ass("_result_", arithSingleLetterScore)) ]

    let DLS =
        [ (0, Ass("_result_", arithDoubleLetterScore)) ]

    let TLS =
        [ (0, Ass("_result_", arithTripleLetterScore)) ]

    let DWS =
        [ (1, Ass("_result_", arithDoubleWordScore)) ]
        @ SLS

    let TWS =
        [ (1, Ass("_result_", arithTripleWordScore)) ]
        @ SLS

    let partiallyApplyScore (word: word) (pos: int) (sq: square) : (int * (int -> int)) list =
        sq
        |> List.map (fun (score: int, f: squareFun) -> (score, (fun (acc: int) -> f word pos acc)))

    let oldCalculatePoints (squares: square list) (w: word) : int =
        squares
        |> List.mapi (fun (pos: int) (sq: square) -> partiallyApplyScore w pos sq)
        |> List.fold (fun st sq -> sq @ st) []
        |> List.sortBy fst
        |> List.map snd
        |> List.fold (fun (f: int -> int) (g: int -> int) -> f >> g) id
        <| 0

    let calculatePoints2 (squares: square2 list) (w: word) : int =
        oldCalculatePoints
            (List.map (fun (sq) -> List.map (fun (pos, stm) -> (pos, stmntToSquareFun (stm))) sq) squares)
            w
