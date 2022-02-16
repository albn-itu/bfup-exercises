module Assignment3.Tests.ArithemticTests

open NUnit.Framework

open Assignment3.Assignment3

[<Test>]
let arithEvalSimple_1() =
    let expected = 42
    let a1 = N 42
    let actual = arithEvalSimple a1

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let arithEvalSimple_2() =
    let expected = 3
    let a2 = N 4 .+. (N 5 .-. N 6)
    let actual = arithEvalSimple a2

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let arithEvalSimple_3() =
    let expected = 42
    let a3 = N 4 .*. N 2 .+. N 34 
    let actual = arithEvalSimple a3

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let arithEvalSimple_4() =
    let expected = 204
    let a4 = (N 4 .+. N 2) .*. N 34
    let actual = arithEvalSimple a4

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let arithEvalSimple_5() =
    let expected = 72
    let a5 = N 4 .+. (N 2 .*. N 34)
    let actual = arithEvalSimple a5

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let arithEvalSimple_6_1() =
    let expected = 5
    let a6 = V "x" 
    let actual = arithEvalState a6 (Map.ofList [("x", 5)])

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let arithEvalSimple_6_2() =
    let expected = 0
    let a6 = V "x" 
    let actual = arithEvalState a6 (Map.ofList [("y", 5)])

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let arithEvalSimple_7_1() =
    let expected = 9
    let a7 = N 4 .+. (V "y" .-. V "z") 
    let actual = arithEvalState a7 (Map.ofList [("x", 4); ("y", 5)])

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let arithEvalSimple_7_2() =
    let expected = 3
    let a7 = N 4 .+. (V "y" .-. V "z") 
    let actual = arithEvalState a7 (Map.ofList [("y", 4); ("z", 5)])

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let arithEval_1() =
    let expected = 0
    let actual = arithEval WL [] Map.empty 

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let wordLengthGivenHelloReturns5() =
    let expected = 5
    let actual = arithEval WL hello Map.empty

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let pointValueOfHinHelloRetursn4() =
    let expected = 4
    let actual = arithEval (PV (N 0)) hello Map.empty 

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let arithEval_2() =
    let expected = 1
    let actual = arithEval arithSingleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 0)])

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let arithEval_3() =
    let expected = 43
    let actual = arithEval arithSingleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 42)])

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let arithEval_4() =
    let expected = 2
    let actual = arithEval arithDoubleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 0)]) 

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let arithEval_5() =
    let expected = 44
    let actual = arithEval arithDoubleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 42)]) 

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let arithEval_6() =
    let expected = 3
    let actual = arithEval arithTripleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 0)]) 

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let arithEval_7() =
    let expected = 45
    let actual = arithEval arithTripleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 42)]) 

    Assert.That(actual, Is.EqualTo(expected))
