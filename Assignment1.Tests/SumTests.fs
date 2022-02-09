module Assignment1.Tests.SumTests

open System
open NUnit.Framework

open Assignment1

[<Test>]
let SumGiven1Returns1() =
  let expected = 1
  let actual = Assignment1.sum(1)
  Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let SumGiven2Returns3() =
  let expected = 3
  let actual = Assignment1.sum(2)
  Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let SumGiven4Returns10() =
  let expected = 10
  let actual = Assignment1.sum(4)
  Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let SumGiven6Returns21() =
  let expected = 21
  let actual = Assignment1.sum(6)
  Assert.That(actual, Is.EqualTo(expected))
