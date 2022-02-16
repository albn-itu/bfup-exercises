module Assignment3.Tests.BoolEval

open NUnit.Framework

open Assignment3.Assignment3

[<Test>]
let boolEval_1() =
    let expected = true
    let actual = boolEval TT [] Map.empty
    
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let boolEval_2() =
    let expected = false
    let actual = boolEval FF [] Map.empty
    
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let boolEval_3() =
    let expected = true
    let actual = boolEval ((V "x" .+. V "y") .=. (V "y" .+. V "x")) [] (Map.ofList [("x", 5); ("y", 7)])

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let boolEval_4() =
    let expected = false
    let actual = boolEval ((V "x" .+. V "y") .=. (V "y" .-. V "x")) [] (Map.ofList [("x", 5); ("y", 7)])

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let boolEval_5() =
    let expected = true
    let actual = boolEval (IsLetter (CV (V "x"))) hello (Map.ofList [("x", 4)])

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let boolEval_6() =
    let expected = false
    let actual = boolEval (IsLetter (CV (V "x"))) (('1', 0)::hello) (Map.ofList [("x", 0)])

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let boolEval_7() =
    let expected = false
    let actual = boolEval (IsDigit (CV (V "x"))) hello (Map.ofList [("x", 4)])

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let boolEval_8() =
    let expected = true
    let actual = boolEval (IsDigit (CV (V "x"))) (('1', 0)::hello) (Map.ofList [("x", 0)])

    Assert.That(actual, Is.EqualTo(expected))
