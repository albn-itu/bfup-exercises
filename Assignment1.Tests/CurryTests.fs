module Assignment1.Tests.CurryTests

open System
open NUnit.Framework

open Assignment1

[<Test>]
let CurryGivenMethodAndInputReturnsOutput() =
  let expected = 8
  let actual = Assignment1.curry (fun(x,y) -> x + y) 5 3

  Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let UncurryGivenMethodAndInputReturnsOutput() =
  let expected = 8
  let actual = Assignment1.uncurry (fun x y -> x + y) (5, 3)

  Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let UncurryGivenTimeDiffReturn() =
  let expected = -59
  let actual = Assignment1.uncurry Assignment1.timediff ((12, 34), (11, 35))

  Assert.That(actual, Is.EqualTo(expected))