module Assignment2.Tests.CombinePairTests

open NUnit.Framework

open Assignment2

[<Test>]
let combinePairGivenEmptyListReturnsEmptyList() =
    let expected = Seq.empty<int * int> 
    let actual = Assignment2.combinePair ([] : int list) 
    
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let combinePairGiven2ElementListReturnsList() =
    let expected = [(true,false)]
    let actual = Assignment2.combinePair [true; false]
    
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let combinePairGivenListReturnsList() =
    let expected = [("Marry", "had"); ("a", "little"); ("lamb", "its"); ("fleece", "was"); ("white", "as")]
    let actual = Assignment2.combinePair ["Marry"; "had"; "a"; "little"; "lamb"; "its"; "fleece"; "was"; "white"; "as"; "snow"]
    
    Assert.That(actual, Is.EqualTo(expected))