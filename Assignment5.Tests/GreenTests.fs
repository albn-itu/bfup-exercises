module Assignment5.Tests.GreenTests

open NUnit.Framework
open Assignment5

[<TestCase(0, 10, 55)>]
[<TestCase(100, 10, 1155)>]
[<TestCase(42, 2345, 2849217)>]
[<TestCase(0, 1000000, 1784293664)>]
let SumGiven100And10Returns1155 x y z =
    let expected = z
    let actual = sum x y
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let LengthGivenEmptyListReturns0 () =
    let expected = 0
    let actual = length []
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let LengthGivenNonEmptyListReturnsCorrectLength () =
    let expected = 3
    let actual = length [ 1; 2; 3 ]
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let FoldBack () =
    let expected = "54321"

    let actual =
        foldBack (fun (x: int) (y: string) -> y + x.ToString()) [ 1; 2; 3; 4; 5 ] ""

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let FactC () =
    let expected = 120
    let actual = factC 5
    Assert.That(actual, Is.EqualTo(expected))

[<TestCase(2, 1)>]
[<TestCase(3, 2)>]
[<TestCase(4, 3)>]
[<TestCase(5, 5)>]
[<TestCase(6, 8)>]
[<TestCase(7, 13)>]
[<TestCase(8, 21)>]
[<TestCase(9, 34)>]
[<TestCase(40, 102334155)>]
let FibAGivenXReturnsY (x, y) =
    let expected = y
    let actual = fibA x

    Assert.That(actual, Is.EqualTo(expected))

[<TestCase(2, 1)>]
[<TestCase(3, 2)>]
[<TestCase(4, 3)>]
[<TestCase(5, 5)>]
[<TestCase(6, 8)>]
[<TestCase(7, 13)>]
[<TestCase(8, 21)>]
[<TestCase(9, 34)>]
let FibCGivenXReturnsY (x, y) =
    let expected = y
    let actual = fibC x

    Assert.That(actual, Is.EqualTo(expected))
