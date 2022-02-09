module Assignment1.Tests.SqrTests

open System
open NUnit.Framework

open Assignment1

[<Test>]
let SqrGiven2Returns4() =
  let expected = 4
  let actual = Assignment1.sqr(2)
  Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let SqrGiven4Returns16() =
  let expected = 16
  let actual = Assignment1.sqr(4)
  Assert.That(actual, Is.EqualTo(expected))

