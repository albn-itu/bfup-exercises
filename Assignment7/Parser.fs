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
let pdo = pstring "pdo"
let pdeclare = pstring "declare"

let pletter = asciiLetter <?> "letter"
let palphanumeric = asciiLetter <|> digit <?> "alphanumeric"

let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"

let spaces = many whitespaceChar
let spaces1 = many1 whitespaceChar

let (.>*>.) (p1: Parser<'a>) (p2: Parser<'b>) = p1 .>> spaces .>>. p2
let (.>*>) (p1: Parser<'a>) (p2: Parser<'b>) = p1 .>> spaces .>> p2
let (>*>.) (p1: Parser<'a>) (p2: Parser<'b>) = p1 .>> spaces >>. p2

let parenthesise p = pchar '(' >*>. p .>*> pchar ')'

let charListToString (c: char, cs: list<char>) : string =
    c :: cs |> List.toArray |> System.String

let pid =
    (pchar '_' <|> pletter)
    .>>. (many (palphanumeric <|> pchar '_'))
    |>> charListToString

let unop (p1: Parser<'a>) (p2: Parser<'b>) = p1 >*>. p2
let binop a p1 p2 = p1 .>*> a .>*>. p2
let methodOp (method: Parser<'a>) (argParser: Parser<'b>) = unop method (parenthesise argParser)

let TermParse, tref = createParserForwardedToRef<aExp> ()
let ProdParse, pref = createParserForwardedToRef<aExp> ()
let AtomParse, aref = createParserForwardedToRef<aExp> ()
let CharParse, cref = createParserForwardedToRef<cExp> ()

let TermBinOp c f =
    binop (pchar c) ProdParse TermParse |>> f

let AddParse = TermBinOp '+' Add <?> "Add"
let SubParse = TermBinOp '-' Sub <?> "Sub"

do tref := choice [ AddParse; SubParse; ProdParse ]

let ProdBinOp c f =
    binop (pchar c) AtomParse ProdParse |>> f

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

let PointParse =
    methodOp pPointValue TermParse |>> PV
    <?> "PointValue"

let VariableParse = pid |>> V <?> "Variable"

let NParse = pint32 |>> N <?> "Int"

let ParParse = parenthesise TermParse

let CharToInParse =
    methodOp pCharToInt CharParse |>> CharToInt
    <?> "CharToInt"

do
    aref
    := choice [ NegParse
                PointParse
                CharToInParse
                VariableParse
                NParse
                ParParse ]
// negation, point value, variables, and integer literals

let AexpParse = TermParse

let CParse =
    pchar '\'' >>. anyChar .>> pchar '\'' |>> C
    <?> "CV"

let CharValueParse =
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
    := choice [ CharValueParse
                IntToCharParse
                ToUpperParse
                ToLowerParse
                CParse ]

let CexpParse = CharParse

let BexpParse = pstring "not implemented"

let stmntParse = pstring "not implemented"

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

let parseSquareProg _ = failwith "not implemented"

let parseBoardProg _ = failwith "not implemented"

type boardFun2 = coord -> StateMonad.Result<square option, StateMonad.Error>

type board =
    { center: coord
      defaultSquare: square
      squares: boardFun2 }

let mkBoard (bp: boardProg) = failwith "not implemented"
