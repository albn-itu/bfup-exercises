module Exam.Tests.First

open NUnit.Framework
open Exam.FirstQuestion

[<Test>]
let q11 () =
    Assert.AreEqual(3, sumMap List.length String.length (Left [1; 2; 3]))
    Assert.AreEqual(12, sumMap List.length String.length (Right "Hello World!"))

[<Test>]
let q12 () =
    Assert.AreEqual(CLeft ("Hello", CRight ([1; 2; 3], CLeft (" world!!!", Nil))),  ofList [Left "Hello"; Right [1; 2; 3]; Left " world!!!"])

    Assert.AreEqual(CLeft (" world!!!", CRight ([1; 2; 3], CLeft ("Hello", Nil))), reverse (CLeft ("Hello", CRight ([1; 2; 3], CLeft (" world!!!", Nil)))))

[<Test>]
let q14 () =
    Assert.AreEqual(CLeft ("Hello", CRight ([1; 2; 3], CLeft (" world!!!", Nil))),  ofList2 [Left "Hello"; Right [1; 2; 3]; Left " world!!!"])

[<Test>]
let q15 () =
    let coll = CLeft ("Hello", (CRight ([1; 2; 3], (CRight ([42], Nil)))))

    Assert.AreEqual(7, foldBackSumColl (fun s acc -> String.length s + acc) (fun lst acc -> List.length lst - acc) coll 0)

