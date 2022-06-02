module Exam.Tests

open NUnit.Framework

[<Test>]
let q35 () =
    Assert.AreEqual([1;1;1;3;2;2;1], elFromString2 "1113221")

[<Test>]
let q45 () =
    Assert.AreEqual([1;2;3;4;5], [1;2;3;4;5] |> ringFromList |> evalSM (iterRemoveSumEven 0u) |> Option.get |> snd |> ringToList)

    let t = [1;2;3;4;5] |> ringFromList |> evalSM (iterRemoveSumEven 10u) |> Option.get |> snd |> ringToList
    printfn $"%A{t}"
    Assert.AreEqual([3], [1;2;3;4;5] |> ringFromList |> evalSM (iterRemoveSumEven 10u) |> Option.get |> snd |> ringToList)
    Assert.AreEqual([5; 6; 1; 2; 3; 4], [1;2;3;4;5;6] |> ringFromList |> evalSM (iterRemoveSumEven 10u) |> Option.get |> snd |> ringToList)