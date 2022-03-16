module ImpParser

open Eval
open StateMonad
(*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

open JParsec.TextParser // Example parser combinator library. Use for CodeJudge.
// open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.

let pIntToChar = pstring "intToChar"
let pPointValue = pstring "pointValue"

let pCharToInt = pstring "charToInt"
let pToUpper = pstring "toUpper"
let pToLower = pstring "toLower"
let pCharValue = pstring "charValue"

let pTrue = pstring "true"
let pFalse = pstring "false"
let pIsDigit = pstring "isDigit"
let pIsLetter = pstring "isLetter"
let pIsVowel = pstring "isVowel"

let pif = pstring "if"
let pthen = pstring "then"
let pelse = pstring "else"
let pwhile = pstring "while"
let pdo = pstring "do"
let pdeclare = pstring "declare"

let pletter = asciiLetter <?> "letter"
let palphanumeric = asciiLetter <|> digit <?> "alphanumeric"

let whitespaceChar =
    satisfy System.Char.IsWhiteSpace <?> "whitespace"

let spaces = many whitespaceChar <?> "space"
let spaces1 = many1 whitespaceChar <?> "space1"

let (.>*>.) (p1: Parser<'a>) (p2: Parser<'b>) = p1 .>> spaces .>>. p2
let (.>*>) (p1: Parser<'a>) (p2: Parser<'b>) = p1 .>> spaces .>> p2
let (>*>.) (p1: Parser<'a>) (p2: Parser<'b>) = p1 .>> spaces >>. p2

let parenthesise p = pchar '(' >*>. p .>*> pchar ')'
let brackethise p = pchar '{' >*>. p .>*> pchar '}'

let charListToString (c: char, cs: list<char>) : string =
    c :: cs |> List.toArray |> System.String

let pid =
    (pchar '_' <|> pletter)
    .>>. (many (palphanumeric <|> pchar '_'))
    |>> charListToString
    <?> "pid"

let unop (p1: Parser<'a>) (p2: Parser<'b>) = p1 >*>. p2 <?> "unop"
let binop a p1 p2 = p1 .>*> a .>*>. p2 <?> "binop"

let methodOp (method: Parser<'a>) (argParser: Parser<'b>) =
    unop method (parenthesise argParser)
    <?> "methodOp"

// ---------------------------------
// Exercise 7.8
// ---------------------------------
let TermParse, tref = createParserForwardedToRef<aExp> ()
let ProdParse, pref = createParserForwardedToRef<aExp> ()
let AtomParse, aref = createParserForwardedToRef<aExp> ()
let CharParse, cref = createParserForwardedToRef<cExp> ()

let TermBinOp c f =
    binop (pchar c) ProdParse TermParse |>> f
    <?> "TermBinOp"

let AddParse = TermBinOp '+' Add <?> "Add"
let SubParse = TermBinOp '-' Sub <?> "Sub"

do tref := choice [ AddParse; SubParse; ProdParse ]

let ProdBinOp c f =
    binop (pchar c) AtomParse ProdParse |>> f
    <?> "ProdBinOp"

let MulParse = ProdBinOp '*' Mul <?> "Mul"
let DivParse = ProdBinOp '/' Div <?> "Div"
let ModParse = ProdBinOp '%' Mod <?> "Mod"

do
    pref
    := choice [ MulParse
                DivParse
                ModParse
                AtomParse ]

let NegParse =
    unop (pchar '-') TermParse
    |>> (fun a -> Mul(N -1, a))
    <?> "Neg"

let PVParse =
    methodOp pPointValue TermParse |>> PV
    <?> "PointValue"

let VParse = pid |>> V <?> "Variable"

let NParse = pint32 |>> N <?> "Int"

let ParParse = parenthesise TermParse <?> "Parentheses"

let CharToIntParse =
    methodOp pCharToInt CharParse |>> CharToInt
    <?> "CharToInt"

do
    aref
    := choice [ NegParse
                PVParse
                CharToIntParse
                VParse
                NParse
                ParParse ]

let AexpParse = TermParse

// ---------------------------------
// Exercise 7.9
// ---------------------------------
let CParse =
    pchar '\'' >>. anyChar .>> pchar '\'' |>> C
    <?> "CV"

let CVParse =
    methodOp pCharValue AexpParse |>> CV
    <?> "CharValue"

let ToUpperParse =
    methodOp pToUpper CharParse |>> ToUpper
    <?> "ToUpper"

let ToLowerParse =
    methodOp pToLower CharParse |>> ToLower
    <?> "ToLower"

let IntToCharParse =
    methodOp pIntToChar AexpParse |>> IntToChar
    <?> "IntToChar"

do
    cref
    := choice [ CVParse
                IntToCharParse
                ToUpperParse
                ToLowerParse
                CParse ]

let CexpParse = CharParse

// ---------------------------------
// Exercise 7.10
// ---------------------------------
let ConjunctionParse, conref = createParserForwardedToRef<bExp> ()
let EqualityParse, eqref = createParserForwardedToRef<bExp> ()
let BMethodParse, bmref = createParserForwardedToRef<bExp> ()

let ConjParse =
    binop (pstring "/\\") EqualityParse ConjunctionParse
    |>> Conj
    <?> "Conjunction"

let CreateDisj (a: bExp, b: bExp) = (Not a, Not b) |> Conj |> Not

let DisjParse =
    binop (pstring "\\/") EqualityParse ConjunctionParse
    |>> (fun x -> CreateDisj x)
    <?> "Disjunction"

do
    conref
    := choice [ ConjParse
                DisjParse
                EqualityParse ]

let AEqParse =
    binop (pchar '=') AexpParse AexpParse |>> AEq
    <?> "Equal"

let ANEqParse =
    binop (pstring "<>") AexpParse AexpParse
    |>> (fun x -> x |> AEq |> Not)
    <?> "NotEqual"

let ALtParse =
    binop (pchar '<') AexpParse AexpParse |>> ALt
    <?> "LessThan"

// This works, but it's as cursed as it gets
let ALtOrEqParse =
    binop (pstring "<=") AexpParse AexpParse
    |>> (fun x -> (ALt x, AEq x |> Not |> Not) |> CreateDisj)
    <?> "LessThanOrEqual"

let AGtParse =
    binop (pchar '>') AexpParse AexpParse
    |>> (fun x -> (AEq x |> Not, ALt x |> Not) |> Conj)
    <?> "GreaterThan"

let AGtOrEqParse =
    binop (pstring ">=") AexpParse AexpParse
    |>> (fun x -> ALt x |> Not)
    <?> "GreaterThanOrEqual"


do
    eqref
    := choice [ AEqParse
                ANEqParse
                ALtParse
                ALtOrEqParse
                AGtParse
                AGtOrEqParse
                BMethodParse ]

let NotParse =
    pchar '~' >>. ConjunctionParse |>> Not <?> "Not"

let IsDigitParse =
    methodOp pIsDigit CexpParse |>> IsDigit
    <?> "IsDigit"

let IsLetterParser =
    methodOp pIsDigit CexpParse |>> IsLetter
    <?> "IsLetter"

let IsVowellParse =
    methodOp pIsDigit CexpParse |>> IsVowel
    <?> "IsVowel"

let TTParse = pTrue |>> (fun _ -> TT) <?> "True"
let FFParse = pFalse |>> (fun _ -> FF) <?> "False"

let ParBParse =
    parenthesise ConjunctionParse <?> "BParentheses"

do
    bmref
    := choice [ NotParse
                IsDigitParse
                IsLetterParser
                IsVowellParse
                TTParse
                FFParse
                ParBParse ]

let BexpParse = ConjunctionParse

// ---------------------------------
// Exercise 7.11
// ---------------------------------
let TopLevelParser, tsref = createParserForwardedToRef<stm> ()
let StatementParser, sref = createParserForwardedToRef<stm> ()

let SemicolonParser =
    StatementParser .>*> pchar ';'
    .>*>. TopLevelParser
    |>> Seq
    <?> "Semicolon"

do
    tsref
    := choice [ SemicolonParser
                StatementParser ]

let AssignParser =
    pid .>*> pstring ":=" .>*>. AexpParse |>> Ass
    <?> "Assign"

let DeclareParser =
    pdeclare >>. spaces1 >>. pid |>> Declare
    <?> "Declare"

let getIf =
    methodOp pif BexpParse .>*> pthen
    .>*>. brackethise StatementParser

let IfParser =
    getIf |>> (fun (b, s1) -> ITE(b, s1, Skip))
    <?> "If"

let IfElseParser =
    getIf .>*> pelse .>*>. brackethise StatementParser
    |>> (fun ((b, s1), s2) -> ITE(b, s1, s2))
    <?> "IfElse"

let WhileParser =
    methodOp pwhile BexpParse .>*> pdo
    .>*>. brackethise StatementParser
    |>> While
    <?> "While"

do
    sref
    := choice [ AssignParser
                DeclareParser
                IfElseParser
                IfParser
                WhileParser ]

let stmntParse = TopLevelParser

(* These five types will move out of this file once you start working on the project *)
type coord = int * int
type squareProg = Map<int, string>

type boardProg =
    { prog: string
      squares: Map<int, squareProg>
      usedSquare: int
      center: coord

      isInfinite: bool // For pretty-printing purposes only
      ppSquare: string } // For pretty-printing purposes only

type word = (char * int) list
type square = Map<int, squareFun>

let parseSquareProg (sqp: squareProg) : square =
    Map.map
        (fun _ v ->
            v
            |> run stmntParse
            |> getSuccess
            |> stmntToSquareFun)
        sqp

type boardFun2 = coord -> StateMonad.Result<square option, StateMonad.Error>

let parseBoardProg (s: string) (sqs: Map<int, square>) : boardFun2 =
    run stmntParse s
    |> getSuccess
    |> (fun x -> stmntToBoardFun x sqs)

type board =
    { center: coord
      defaultSquare: square
      squares: boardFun2 }

let mkBoard (bp: boardProg) : board =
    let m' =
        Map.map (fun _ v -> parseSquareProg v) bp.squares

    { center = bp.center
      defaultSquare = m'.[bp.usedSquare]
      squares = parseBoardProg bp.prog m' }
