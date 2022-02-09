module Assignment1.Tests.ScrappleTests

open System
open NUnit.Framework

open Assignment1

[<TestCase(0)>]
[<TestCase(42)>]
[<TestCase(-762)>]
let EmptyGivenAnyIntegerReturnsDef(x) =
  let expected = ('A', 1)

  let theLetterA : int -> char * int = Assignment1.empty ('A', 1)
  let actual = theLetterA x

  Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let AddGivenNewPosReturnAddedWord() =
  let theLetterA : int -> char * int = Assignment1.empty ('A', 1)
  let expected = ('B', 3)

  let theLettersAB = Assignment1.add 1 ('B', 3) theLetterA
  let actual = theLettersAB 1

  Assert.That(actual, Is.EqualTo(expected))

[<TestCase(0)>]
[<TestCase(42)>]
let AddGivenXReturnsDefault(x) =
  let theLetterA : int -> char * int = Assignment1.empty ('A', 1)
  let expected = ('A', 1)

  let theLettersAB = Assignment1.add 1 ('B', 3) theLetterA
  let actual = theLettersAB x

  Assert.That(actual, Is.EqualTo(expected))

[<TestCase(0, 'H')>]
[<TestCase(1, 'E')>]
[<TestCase(2, 'L')>]
[<TestCase(3, 'L')>]
[<TestCase(4, 'O')>]
[<TestCase(49, '\000')>]
let HelloGivenPosReturnsChar(x, y) =
  let expected = y
  let (actual, _) = Assignment1.hello x

  Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let SingleLetterScoreGivenHelloReturns1() =
  let expected = 1
  let actual = Assignment1.singleLetterScore Assignment1.hello 4

  Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let DoubleLetterScoreGivenHelloReturns2() =
  let expected = 2
  let actual = Assignment1.doubleLetterScore Assignment1.hello 4

  Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let TripleLetterScoreGivenHelloReturns2() =
  let expected = 3
  let actual = Assignment1.tripleLetterScore Assignment1.hello 4

  Assert.That(actual, Is.EqualTo(expected))

