module Assignment2.Tests.StringTests

open NUnit.Framework

open Assignment2

[<Test>]
let explodeGivenEmptyStringReturnsEmptyList() =
    let expected = Seq.empty<char> 
    let actual = Assignment2.explode1 ""
    
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let explodeGivenHelloWorldReturnsList() =
    let expected = ['H'; 'e'; 'l'; 'l'; 'o'; ' '; 'W'; 'o'; 'r'; 'l'; 'd'; '!']
    let actual = Assignment2.explode1 "Hello World!"
    
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let explode2GivenEmptyStringReturnsEmptyList() =
    let expected = Seq.empty<char> 
    let actual = Assignment2.explode2 ""
    
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let explode2GivenHelloWorldReturnsList() =
    let expected = ['H'; 'e'; 'l'; 'l'; 'o'; ' '; 'W'; 'o'; 'r'; 'l'; 'd'; '!']
    let actual = Assignment2.explode2 "Hello World!"
    
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let implodeGivenEmptyListReturnsEmptyString() =
    let expected = ""
    let actual = Assignment2.implode []
    
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let implodeGivenHelloWorldReturnsString() =
    let expected = "Hello World!"
    let actual = Assignment2.implode ['H'; 'e'; 'l'; 'l'; 'o'; ' '; 'W'; 'o'; 'r'; 'l'; 'd'; '!']
    
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let implodeRevGivenEmptyListReturnsEmptyString() =
    let expected = ""
    let actual = Assignment2.implodeRev []
    
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let implodeRevGivenHelloWorldReturnsString() =
    let expected = "!dlroW olleH"
    let actual = Assignment2.implodeRev ['H'; 'e'; 'l'; 'l'; 'o'; ' '; 'W'; 'o'; 'r'; 'l'; 'd'; '!']
    
    Assert.That(actual, Is.EqualTo(expected))

[<Test>]
let toUpperGivenHelloWorldReturnsString() =
    let expected = "HELLO WORLD!"
    let actual = Assignment2.toUpper "Hello World!"
    
    Assert.That(actual, Is.EqualTo(expected))
