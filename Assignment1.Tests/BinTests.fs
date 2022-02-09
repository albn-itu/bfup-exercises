module Assignment1.Tests.BinTests

open System
open NUnit.Framework

open Assignment1

[<TestCase(2, 1, 2)>]
[<TestCase(4, 2, 6)>]
let BinGivenXAndYReturnsZ(x, y, z) =
  let expected = z
  let actual = Assignment1.bin(x,y)

  Assert.That(actual, Is.EqualTo(expected))
