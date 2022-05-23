module Exam.Tests.FirstTest

open NUnit.Framework

open Exam.FirstQuestion

[<Test>]
let toInt1 () =
    Assert.AreEqual(0u, toInt O)

[<Test>]
let toInt2 () =
    Assert.AreEqual(3u, toInt (S (S (S O))))

[<Test>]
let toInt3 () =
    Assert.AreEqual(6u, toInt (S (S (S (S (S (S O)))))))

[<Test>]
let fromInt1 () =
    Assert.AreEqual(O, fromInt 0u)

[<Test>]
let fromInt2 () =
    Assert.AreEqual((S (S (S O))), fromInt 3u)

[<Test>]
let fromInt3 () =
    Assert.AreEqual((S (S (S (S (S (S O)))))), fromInt 6u)

[<Test>]
let add () =
    Assert.AreEqual(S (S (S (S (S O)))), add  (S (S O)) (S (S (S O))))

[<Test>]
let mul () =
    Assert.AreEqual(S (S (S (S (S (S O))))), mult (S (S O)) (S (S (S O))))

[<Test>]
let pow () =
    Assert.AreEqual(S (S (S (S (S (S (S (S O))))))), pow (S (S O)) (S (S (S O))))

[<Test>]
let tailAdd () =
    Assert.AreEqual(S (S (S (S (S O)))), tailAdd  (S (S O)) (S (S (S O))))

[<Test>]
let tailMul () =
    Assert.AreEqual(S (S (S (S (S (S O))))), tailMult (S (S O)) (S (S (S O))))

[<Test>]
let tailPow () =
    Assert.AreEqual(S (S (S (S (S (S (S (S O))))))), tailPow (S (S O)) (S (S (S O))))

[<Test>]
let loop1 () =
    Assert.AreEqual(true, loop not true O)

[<Test>]
let loop2 () =
    Assert.AreEqual(false, loop not true (S (S (S O))))

[<Test>]
let loop3 () =
    Assert.AreEqual(true, loop not true (S (S (S (S (S (S (S (S O)))))))))

[<Test>]
let loopAdd () =
    Assert.AreEqual(S (S (S (S (S O)))), loopAdd  (S (S O)) (S (S (S O))))

[<Test>]
let loopMul () =
    Assert.AreEqual(S (S (S (S (S (S O))))), loopMult (S (S O)) (S (S (S O))))

[<Test>]
let loopPow () =
    Assert.AreEqual(S (S (S (S (S (S (S (S O))))))), loopPow (S (S O)) (S (S (S O))))

