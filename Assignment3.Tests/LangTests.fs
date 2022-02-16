module Assignment3.Tests.LangTests

open NUnit.Framework

open Assignment3.Assignment3

[<Test>]
let isConsonant_1 () =
    let expected = true

    let actual =
        boolEval (isConsonant (C 'H')) [] Map.empty

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let isConsonant_2 () =
    let expected = true

    let actual =
        boolEval (isConsonant (C 'h')) [] Map.empty

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let isConsonant_3 () =
    let expected = false

    let actual =
        boolEval (isConsonant (C 'A')) [] Map.empty

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let isConsonant_4 () =
    let expected = true

    let actual =
        boolEval (isConsonant (CV(V "x"))) hello (Map.ofList [ ("x", 0) ])

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let isConsonant_5 () =
    let expected = false

    let actual =
        boolEval (isConsonant (CV(V "x"))) hello (Map.ofList [ ("x", 1) ])

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let stmt_1 () =
    let expected = Map.empty
    let actual = evalStmnt Skip [] Map.empty

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let stmt_2 () =
    let expected = Map.ofList [ ("x", 5) ]
    let actual = evalStmnt (Ass("x", (N 5))) [] Map.empty

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let stmt_3 () =
    let expected = Map.ofList [ ("x", 5); ("y", 7) ]

    let actual =
        evalStmnt (Seq(Ass("x", WL), Ass("y", N 7))) hello Map.empty

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let stmt_4 () =
    let expected = Map.ofList [ ("x", 1) ]

    let actual =
        evalStmnt (ITE(WL .>=. N 5, Ass("x", N 1), Ass("x", N 2))) hello Map.empty

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let stmt_5 () =
    let expected = Map.ofList [ ("x", 2) ]

    let actual =
        evalStmnt (ITE(WL .<. N 5, Ass("x", N 1), Ass("x", N 2))) hello Map.empty

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let stmt_6 () =
    let expected = Map.ofList [ ("x", 6); ("y", 15) ]

    let actual =
        evalStmnt (While(V "x" .<=. WL, Seq(Ass("y", V "y" .+. V "x"), Ass("x", V "x" .+. N 1)))) hello Map.empty

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let stmt_7 () =
    let expected = Map.ofList [ ("x", 6); ("y", 112) ]

    let actual =
        evalStmnt
            (While(V "x" .<=. WL, Seq(Ass("y", V "y" .+. V "x"), Ass("x", V "x" .+. N 1))))
            hello
            (Map.ofList [ ("x", 3); ("y", 100) ])

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let squareFun_1 () =
    let expected = 4
    let actual = singleLetterScore hello 0 0

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let squareFun_2 () =
    let expected = 8
    let actual = doubleLetterScore hello 0 0

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let squareFun_3 () =
    let expected = 12
    let actual = tripleLetterScore hello 0 0

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let squareFun_4 () =
    let expected = 46
    let actual = singleLetterScore hello 0 42

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let squareFun_5 () =
    let expected = 50
    let actual = doubleLetterScore hello 0 42

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let squareFun_6 () =
    let expected = 54
    let actual = tripleLetterScore hello 0 42

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let squareFun_7 () =
    let expected = 50
    let actual = containsNumbers hello 5 50

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let squareFun_8 () =
    let expected = -50

    let actual =
        containsNumbers (('0', 100) :: hello) 5 50

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let squareFun_9 () =
    let expected = -50

    let actual =
        containsNumbers (hello @ [ ('0', 100) ]) 5 50

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let oddConsonantsNegateAccIfOddNumberOfConsonantsInWord () =
    let expected = -42

    let actual =
        (stmntToSquareFun oddConsonants) hello 0 42

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let oddConsonantsNotNegateAccIfEvenNumberOfConsonantsInWord () =
    let expected = 42
    let actual = (stmntToSquareFun oddConsonants) [] 0 42

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let calculatePoints_1 () =
    let expected = 28

    let actual =
        calculatePoints2 [ DLS; SLS; TLS; SLS; DWS ] hello

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let calculatePoints_2 () =
    let expected = 168

    let actual =
        calculatePoints2 [ DLS; DWS; TLS; TWS; DWS ] hello

    Assert.That(actual, Is.EqualTo(expected))
