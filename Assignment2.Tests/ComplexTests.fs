module Assignment2.Tests.ComplexTests

open NUnit.Framework

open Assignment2

[<Test>]
let mkComplexGiven2FloatsReturnsComplexType() =
    let expected: Assignment2.complex = (1.0, 2.0)
    let actual = Assignment2.mkComplex 1.0 2.0
    
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let complexToPairGivenComplexTypeReturnsPair() =
    let expected = (1.0, 2.0)
    let actual = Assignment2.complexToPair ((1.0, 2.0): Assignment2.complex)
    
    Assert.That(actual, Is.EqualTo(expected))
