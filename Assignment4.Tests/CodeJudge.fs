module Assignment4.Tests.CodeJudge

open NUnit.Framework

open Dictionary

[<Test>]
let green_4_02_Test07_01 () =
    let readLines filePath = System.IO.File.ReadLines(filePath)

    let fromFile (path: string) =
        readLines path
        |> Seq.fold (fun acc s -> insert s acc) (empty ())

    let dict =
        fromFile "../../../EnglishDictionary.txt"

    Assert.IsTrue(lookup "TONIGHT" dict)

[<Test>]
let yellow_4_02_Test02_1 () =
    let readLines filePath = System.IO.File.ReadLines(filePath)

    let ss =
        readLines "../../../EnglishDictionary.txt"
        |> Seq.toList

    // Insert all
    let dict =
        List.fold (fun acc s -> insert s acc) (empty ()) ss

    // Reverse a string
    let reverse (s: string) =
        Seq.rev s |> Seq.toArray |> System.String

    Assert.IsTrue(lookup (reverse "AA") dict)
    Assert.IsFalse(lookup (reverse "AAH") dict)
    Assert.IsFalse(lookup (reverse "AAL") dict)
    Assert.IsTrue(lookup (reverse "AB") dict)
    Assert.IsTrue(lookup (reverse "ABA") dict)
    Assert.IsTrue(lookup (reverse "ABBA") dict)
    Assert.IsTrue(lookup (reverse "ABO") dict)
