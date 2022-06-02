module Exam.Tests

open NUnit.Framework

[<Test>]
let q11_1 () =
    Assert.AreEqual([ true ], insert true [])
    Assert.AreEqual([ 1; 3; 5; 8; 9 ], insert 5 [ 1; 3; 8; 9 ])
    Assert.AreEqual([ 1; 3; 5; 8; 9 ], insert 9 [ 1; 3; 5; 8 ])
    Assert.AreEqual([ 1; 3; 5; 8; 9 ], insert 8 [ 1; 3; 5; 9 ])
    Assert.AreEqual([ 1; 3; 8; 9 ], insert 3 [ 1; 8; 9 ])

[<Test>]
let q11_2 () =
    Assert.AreEqual([ 1; 3; 5; 8; 9 ], insertionSort [ 5; 3; 1; 9; 8 ])
    Assert.AreEqual([ 1; 3; 5; 8; 9 ], insertionSort [ 5; 3; 1; 8; 9 ])


[<Test>]
let q12_1 () =
    Assert.AreEqual([ true ], insertTail true [])
    Assert.AreEqual([ 1; 3; 5; 8; 9 ], insertTail 5 [ 1; 3; 8; 9 ])
    Assert.AreEqual([ 1; 3; 5; 8; 9 ], insertTail 9 [ 1; 3; 5; 8 ])
    Assert.AreEqual([ 1; 3; 5; 8; 9 ], insertTail 8 [ 1; 3; 5; 9 ])
    Assert.AreEqual([ 1; 3; 8; 9 ], insertTail 3 [ 1; 8; 9 ])

[<Test>]
let q12_2 () =
    Assert.AreEqual([ 1; 3; 5; 8; 9 ], insertionSortTail [ 5; 3; 1; 9; 8 ])
    Assert.AreEqual([ 1; 3; 5; 8; 9 ], insertionSortTail [ 5; 3; 1; 8; 9 ])


[<Test>]
let q13 () =
    Assert.AreEqual([ 1; 3; 5; 8; 9 ], insertionSort2 [ 5; 3; 1; 9; 8 ])
    Assert.AreEqual([ 1; 3; 5; 8; 9 ], insertionSort2 [ 5; 3; 1; 8; 9 ])

[<Test>]
let q14_1 () =
    Assert.AreEqual([ "q"; "bb"; "abc"; "lbcd" ], insertBy String.length "abc" [ "q"; "bb"; "lbcd" ])
    Assert.AreEqual([ "q"; "bb"; "abc"; "lbcd" ], insertionSortBy String.length [ "bb"; "q"; "abc"; "lbcd" ])
