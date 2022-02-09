module Assignment1.Tests.PowTests

open System
open NUnit.Framework

open Assignment1

[<Test>]
let PowGiven2And4Returns16() =
  let expected = 16
  let actual = Assignment1.pow 2 4
  Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let PowGiven5And6Returns15625() =
  let expected = 15625
  let actual = Assignment1.pow 5 6
  Assert.That(actual, Is.EqualTo(expected))
