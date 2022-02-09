module Assignment1.Tests.DupTests

open System
open NUnit.Framework

open Assignment1

[<TestCase("Hi", "HiHi")>]
[<TestCase("Hi ", "Hi Hi ")>]
[<TestCase("LongWord", "LongWordLongWord")>]
let DupGivenXReturnsY(x, y) =
  let expected = y
  let actual = Assignment1.dup(x)
  Assert.That(actual, Is.EqualTo(expected))

[<TestCase("Hi", 2, "HiHi")>]
[<TestCase("Hi ", 4, "Hi Hi Hi Hi ")>]
[<TestCase("LongWord", 3, "LongWordLongWordLongWord")>]
let DupnGivenXAndYReturnsZ(x, y, z) =
  let expected = z
  let actual = Assignment1.dupn x y
  Assert.That(actual, Is.EqualTo(expected))


