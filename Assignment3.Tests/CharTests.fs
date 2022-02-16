module Assignment3.Tests.CharTests

open NUnit.Framework

open Assignment3.Assignment3

[<Test>]
let charEval_1() =
    let expected = 'H'
    let actual = charEval (C 'H') [] Map.empty

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let charEval_2() =
    let expected = 'h'
    let actual = charEval (ToLower (CV (N 0))) hello Map.empty

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let charEval_3() =
    let expected = 'H'
    let actual = charEval (ToUpper (C 'h')) [] Map.empty

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let charEval_4() =
    let expected = '*'
    let actual = charEval (ToLower (C '*')) [] Map.empty

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let charEval_5() =
    let expected = 'O'
    let actual = charEval (CV (V "x" .-. N 1)) hello (Map.ofList [("x", 5)])

    Assert.That(actual, Is.EqualTo(expected))
