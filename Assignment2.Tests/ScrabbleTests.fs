module Assignment2.Tests.ScrablleTests

open System
open NUnit.Framework

open Assignment2

[<TestCase(4,0,1)>]
[<TestCase(4,42,43)>]
let singleLetterScoreGivenXYReturnsZ(x, y, z) =
    let expected = z
    let actual = Assignment2.singleLetterScore Assignment2.hello x y
    
    Assert.That(actual, Is.EqualTo(expected))

[<TestCase(4,0,2)>]
[<TestCase(4,42,44)>]
let dobuleLetterScoreGivenXYReturnsZ(x, y, z) =
    let expected = z
    let actual = Assignment2.doubleLetterScore Assignment2.hello x y
    
    Assert.That(actual, Is.EqualTo(expected))

[<TestCase(4,0,3)>]
[<TestCase(4,42,45)>]
let tripleLetterScoreGivenXYReturnsZ(x, y, z) =
    let expected = z
    let actual = Assignment2.tripleLetterScore Assignment2.hello x y
    
    Assert.That(actual, Is.EqualTo(expected))


[<TestCase(4, 0, 0)>]
[<TestCase(12345, 42, 84)>]
let doubleWordScoreGivenXYReturnsZ(x, y, z) =
    let expected = z
    let actual = Assignment2.doubleWordScore Assignment2.hello x y
    
    Assert.That(actual, Is.EqualTo(expected))

[<TestCase(4, 0, 0)>]
[<TestCase(12345, 42, 126)>]
let tripleWordScoreGivenXYReturnsZ(x, y, z) =
    let expected = z
    let actual = Assignment2.tripleWordScore Assignment2.hello x y
    
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let oddConsonantsNegateAccIfOddNumberOfConsonantsInWord() =
    let expected = -42
    let actual = Assignment2.oddConsonants Assignment2.hello 0 42
    
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let oddConsonantsNotNegateAccIfEvenNumberOfConsonantsInWord() =
    let expected = 42
    let actual = Assignment2.oddConsonants [] 0 42
    
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let calculatePointsGivenTilesReturn28() =
    let expected = 28
    let actual = Assignment2.calculatePoints [Assignment2.DLS; Assignment2.SLS; Assignment2.TLS; Assignment2.SLS; Assignment2.DWS]  Assignment2.hello
    
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let calculatePointsGivenTilesReturn168() =
    let expected = 168
    let actual = Assignment2.calculatePoints [Assignment2.DLS; Assignment2.DWS; Assignment2.TLS; Assignment2.TWS; Assignment2.DWS]  Assignment2.hello
    
    Assert.That(actual, Is.EqualTo(expected))

