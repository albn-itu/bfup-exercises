module Assignment1.Tests.TimediffTests

open System
open NUnit.Framework

open Assignment1

[<TestCase(12, 34, 11, 35, -59)>]
[<TestCase(12, 34, 13, 35, 61)>]
let TimediffGivenXAndYReturnsZ(x1, x2, y1, y2, z) =
  let expected = z
  let actual = Assignment1.timediff (x1, x2) (y1, y2)

  Assert.That(actual, Is.EqualTo(expected))

[<TestCase(14, 24, 864)>]
[<TestCase(23, 1, 1381)>]
let MinutesGivenXReturnY(x1, x2, y) = 
  let expected = y
  let actual = Assignment1.minutes (x1, x2)

  Assert.That(actual, Is.EqualTo(expected))


