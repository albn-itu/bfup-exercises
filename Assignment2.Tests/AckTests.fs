module Assignment2.Tests.AckTests

open NUnit.Framework

open Assignment2

[<TestCase(2,1,5)>]
[<TestCase(2,3,9)>]
[<TestCase(3,4,125)>]
[<TestCase(1,1,3)>]
[<TestCase(0,0,1)>]
let ackGivenXAndYReturnsZ(x, y, z) =
    let expected = z
    let actual = Assignment2.ack (x,y)
    
    Assert.That(actual, Is.EqualTo(expected))
