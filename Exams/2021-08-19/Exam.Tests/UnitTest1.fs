module Exam.Tests

open NUnit.Framework

[<Test>]
let q11 () =
    Assert.AreEqual(0, length Nil)
    Assert.AreEqual(5, length (Cons1(3, Cons2(true, Cons1(4, Cons2(false, Cons2(true, Nil)))))))

[<Test>]
let q12 () =
    Assert.AreEqual(
        ([ 3; 4 ], [ true; false; true ]),
        split (Cons1(3, Cons2(true, Cons1(4, Cons2(false, Cons2(true, Nil))))))
    )

    Assert.AreEqual((2, 3), length2 (Cons1(3, Cons2(true, Cons1(4, Cons2(false, Cons2(true, Nil)))))))

[<Test>]
let q13 () =
    let t: binList<bool, int> = Nil

    Assert.AreEqual(
        t,
        map
            (fun x -> x % 2 = 0)
            (function
            | true -> 0
            | false -> 1)
            (Nil: binList<int, bool>)
    )

    Assert.AreEqual(
        Cons1(false, Cons2(0, Cons1(true, Cons2(1, Cons2(0, Nil))))),
        map
            (fun x -> x % 2 = 0)
            (function
            | true -> 0
            | false -> 1)
            (Cons1(3, Cons2(true, Cons1(4, Cons2(false, Cons2(true, Nil))))))
    )

[<Test>]
let q14 () =
    Assert.AreEqual(
        Cons2(true, Cons1(4, Cons2(true, Nil))),
        filter (fun x -> x % 2 = 0) id (Cons1(3, Cons2(true, Cons1(4, Cons2(false, Cons2(true, Nil))))))
    )

[<Test>]
let q15 () =
    Assert.AreEqual(
        -7,
        fold
            (+)
            (fun acc ->
                function
                | true -> acc
                | false -> -acc)
            0
            (Cons1(3, Cons2(true, Cons1(4, Cons2(false, Cons2(true, Nil))))))
    )

[<Test>]
let q31 () =
    Assert.AreEqual(2.0, approxSquare 5 0)
    Assert.AreEqual(2.25, approxSquare 5 1)
    Assert.AreEqual(2.2361111111111112, approxSquare 5 2)
    Assert.AreEqual(2.2360679779158037, approxSquare 5 3)
    Assert.AreEqual(2.2360679774997898, approxSquare 5 4)

[<Test>]
let q33 () =
    Assert.AreEqual((-2.0, 0.75), solveQuadratic "-4x^2 - 5x + 6 = 0" 5)
    Assert.AreEqual((-2.0, 0.75), solveQuadratic "-4x^2    -    5x+ 6=        0" 5)
    Assert.AreEqual((-2.0, 0.75), solveQuadratic "-4x^2-5x+6=0" 5)

[<Test>]
let q42 () =
    Assert.AreEqual("5 / 6", mkRat 5 6 |> Option.get |> ratToString)
    Assert.AreEqual("3 / 2", mkRat 15 10 |> Option.get |> ratToString)
    Assert.AreEqual("-3 / 2", mkRat -15 10 |> Option.get |> ratToString)
    Assert.AreEqual("-3 / 2", mkRat 15 -10 |> Option.get |> ratToString)
    Assert.AreEqual("3 / 2", mkRat -15 -10 |> Option.get |> ratToString)
    Assert.AreEqual("0 / 1", mkRat 0 5 |> Option.get |> ratToString)
    Assert.AreEqual(None, mkRat 5 0)

[<Test>]
let q43 () =
    let r1 = mkRat 2 3 |> Option.get
    let r2 = mkRat 3 4 |> Option.get

    Assert.AreEqual("17 / 12", plus r1 r2 |> Option.get |> ratToString)
    Assert.AreEqual("-1 / 12", minus r1 r2 |> Option.get |> ratToString)
    Assert.AreEqual("0 / 1", minus r2 r2 |> Option.get |> ratToString)
    Assert.AreEqual("1 / 2", mult r1 r2 |> Option.get |> ratToString)
    Assert.AreEqual("8 / 9", div r1 r2 |> Option.get |> ratToString)
    Assert.AreEqual(None, div r1 (minus r2 r2 |> Option.get))

[<Test>]
let q44 () =
    let r1 = mkRat 2 3 |> Option.get
    let r2 = mkRat 3 4 |> Option.get

    Assert.AreEqual(
        "17 / 12",
        r1
        |> evalSM (smPlus r2)
        |> Option.get
        |> snd
        |> ratToString
    )

    Assert.AreEqual(
        "-1 / 12",
        r1
        |> evalSM (smMinus r2)
        |> Option.get
        |> snd
        |> ratToString
    )

    Assert.AreEqual(
        "1 / 2",
        r1
        |> evalSM (smMult r2)
        |> Option.get
        |> snd
        |> ratToString
    )

    Assert.AreEqual(
        "8 / 9",
        r1
        |> evalSM (smDiv r2)
        |> Option.get
        |> snd
        |> ratToString
    )

[<Test>]
let q45 () =
    let r1 = mkRat 2 3 |> Option.get
    let r2 = mkRat 3 4 |> Option.get
    let r3 = mkRat 4 5 |> Option.get
    let r4 = mkRat 5 6 |> Option.get
    let r5 = mkRat 6 7 |> Option.get

    Assert.AreEqual(
        "259 / 432",
        evalSM
            (calculate [ (r2, smPlus)
                         (r3, smMinus)
                         (r4, smMult)
                         (r5, smDiv) ])
            r1
        |> Option.get
        |> snd
        |> ratToString
    )
