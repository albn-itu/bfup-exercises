module Assignment2.Tests.DownToTests

open System
open NUnit.Framework

open Assignment2

[<Test>]
let downTo1Given5ReturnsList() =
    let expected = [5;4;3;2;1]
    let actual = Assignment2.downto1 5

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let downTo2Given10ReturnsList() =
    let expected = [10;9;8;7;6;5;4;3;2;1]
    let actual = Assignment2.downto2 10

    Assert.That(actual, Is.EqualTo(expected)) 

[<Test>]
let downTo1Given0ReturnsEmptyList() =
    let expected = Seq.empty<int> 
    let actual = Assignment2.downto1 0

    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let downTo2GivenNegativeReturnsEmptyList() =
    let expected = Seq.empty<int> 
    let actual = Assignment2.downto2 -42

    Assert.That(actual, Is.EqualTo(expected))

[<TestCase(0, 1)>]
[<TestCase(1, 1)>]
[<TestCase(2, 2)>]
[<TestCase(3, 6)>]
[<TestCase(4, 24)>]
[<TestCase(5, 120)>]
[<TestCase(6, 720)>]
let facGivenXReturnY(x, y) =
    let expected = y
    let actual = Assignment2.fac x

    Assert.That(actual, Is.EqualTo(expected))