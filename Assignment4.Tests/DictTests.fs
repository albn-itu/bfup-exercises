module Assignment4.Tests.DictTests

open NUnit.Framework

open Dictionary

[<Test>]
let lookup_Lookup_Care () =
    let dict = insert "care" (empty ())
    let word = "care"

    let result = lookup word dict
    Assert.IsTrue(result)

[<Test>]
let lookup_Lookup_Cares () =
    let dict = insert "cares" (empty ())
    let word = "cares"

    let result = lookup word dict
    Assert.IsTrue(result)

[<Test>]
let lookup_Lookup_Partial_Word () =
    let dict = insert "cares" (empty ())
    let word = "care"

    let result = lookup word dict
    Assert.IsFalse(result)

[<Test>]
let lookup_Lookup_Not_Existing_Word () =
    let dict = insert "cares" (empty ())
    let word = "carez"

    let result = lookup word dict
    Assert.IsFalse(result)

[<Test>]
let insert_lookup_Many_Words () =
    let mutable dict = empty ()
    dict <- insert "care" dict
    dict <- insert "cares" dict
    dict <- insert "carez" dict
    dict <- insert "hello" dict
    dict <- insert "give me strength" dict
    dict <- insert "give me" dict

    Assert.IsTrue(lookup "care" dict)
    Assert.IsFalse(lookup "car" dict) // Partials are not accepted
    Assert.IsFalse(lookup "give" dict)
    Assert.IsTrue(lookup "hello" dict)
    Assert.IsFalse(lookup "hell" dict)
    Assert.IsTrue(lookup "give me" dict)

    Assert.IsFalse(lookup "caress" dict)
    Assert.IsFalse(lookup "cary" dict)

[<Test>]
let codejudge_bug_1() =
   let res = lookup "HE" (empty () |> insert "HELLO")
   
   Assert.IsFalse(res)