module Assignment1.Tests.FibTests

open System
open NUnit.Framework

open Assignment1

[<TestCase(0, 0)>]
[<TestCase(1, 1)>]
[<TestCase(2, 1)>]
[<TestCase(3, 2)>]
[<TestCase(4, 3)>]
[<TestCase(5, 5)>]
[<TestCase(6, 8)>]
[<TestCase(7, 13)>]
[<TestCase(8, 21)>]
[<TestCase(9, 34)>]
let FibGivenXReturnsY(x, y) =
  let expected = y
  let actual = Assignment1.fib(x)

  Assert.That(actual, Is.EqualTo(expected))
