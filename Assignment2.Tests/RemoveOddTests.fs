module Assignment2.Tests.RemoveOddTests

open NUnit.Framework

open Assignment2

[<Test>]
let removeOddIdXGivenEmptyListReturnsEmptyList() =
    let expected = Seq.empty<int> 
    let actual = Assignment2.removeOddIdx ([] : int list) 
    
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let removeOddIdXGivenListWithOneElementReturnsList() =
    let expected = [true]
    let actual = Assignment2.removeOddIdx [true]
    
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let removeOddIdXGivenListReturnsList() =
    let expected = ["Marry"; "a"; "lamb"; "fleece"; "white"; "snow"]
    let actual = Assignment2.removeOddIdx ["Marry"; "had"; "a"; "little"; "lamb"; "its"; "fleece"; "was"; "white"; "as"; "snow"]

    TestContext.Out.WriteLine(actual)

    Assert.That(actual, Is.EqualTo(expected))
