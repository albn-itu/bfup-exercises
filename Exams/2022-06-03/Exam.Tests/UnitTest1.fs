module Exam.Tests

open NUnit.Framework

open Exam2022


let img =
    Quad (
        Square 255uy,
        Square 128uy,
        Quad(
            Square 255uy,
            Square 128uy,
            Square 192uy,
            Square 64uy
        ),
        Square 0uy
    )

[<Test>]
let q11 () =
    Assert.AreEqual(0, countWhite (Square 123uy))
    Assert.AreEqual(2, countWhite img)

[<Test>]
let q12 () =
    Assert.AreEqual(Square 123uy, rotateRight (Square 123uy))
    Assert.AreEqual(Quad (Square 255uy, Square 0uy, Square 85uy, Square 170uy), rotateRight (Quad(Square 0uy, Square 85uy, Square 170uy, Square 255uy)))
    Assert.AreEqual(
        Quad(
            Square 0uy,
            Square 255uy,
            Square 128uy,
            Quad (
                Square 64uy,
                Square 255uy,
                Square 128uy,
                Square 192uy
            )),
        rotateRight img
    )

[<Test>]
let q13map () =
    Assert.AreEqual(Square 10uy, map (fun x -> Square (x + 10uy)) (Square 0uy))
    Assert.AreEqual(Quad (Square 10uy, Square 95uy, Square 180uy, Square 9uy), map (fun x -> Square (x + 10uy))(Quad (Square 0uy, Square 85uy, Square 170uy, Square 255uy)))
    Assert.AreEqual(
        Quad (Square 133uy, Square 143uy, Square 153uy, Square 163uy),
        map (fun x -> Quad (
            Square (x + 10uy),
            Square (x + 20uy),
            Square (x + 30uy),
            Square (x + 40uy))
        ) (Square (123uy))
    )

[<Test>]
let q13bitmap () =
    Assert.AreEqual(Square 0uy, bitmap (Square 120uy))
    Assert.AreEqual(Square 255uy, bitmap (Square 150uy))
    Assert.AreEqual(
        Quad(
            Square 255uy,
            Square 255uy,
            Quad (Square 255uy, Square 255uy, Square 255uy, Square 0uy),
            Square 0uy
        ), 
        bitmap img
    )

[<Test>]
let q14fold () =
    Assert.AreEqual(123, fold (fun acc x -> acc + int x) 0 (Square 123uy))
    Assert.AreEqual(510, fold (fun acc x -> acc + int x) 0 (Quad (Square 0uy, Square 85uy, Square 170uy, Square 255uy)))
    Assert.AreEqual(1022, fold (fun acc x -> acc + int x) 0 img)

[<Test>]
let q14white () =
    Assert.AreEqual(0, countWhite2 (Square 123uy))
    Assert.AreEqual(2, countWhite2 img)

[<Test>]
let q22 () = 
    Assert.AreEqual(foo 0, foo2 0)
    Assert.AreEqual(foo 1, foo2 1)
    Assert.AreEqual(foo 2, foo2 2)
    Assert.AreEqual(foo 3, foo2 3)
    Assert.AreEqual(foo 5, foo2 5)
    Assert.AreEqual(foo 10, foo2 10)
    Assert.AreEqual(foo 20, foo2 20)

[<Test>]
let q23 () =
    Assert.AreEqual(bar [0;1;2;3], bar2 [0;1;2;3])
    Assert.AreEqual(bar [10;20;30], bar2 [10;20;30])
    Assert.AreEqual(bar [100;25;35], bar2 [100;25;35])

[<Test>]
let q25 () = 
    Assert.AreEqual(foo 0, fooTail 0)
    Assert.AreEqual(foo 1, fooTail 1)
    Assert.AreEqual(foo 2, fooTail 2)
    Assert.AreEqual(foo 3, fooTail 3)
    Assert.AreEqual(foo 5, fooTail 5)
    Assert.AreEqual(foo 10, fooTail 10)
    Assert.AreEqual(foo 20, fooTail 20)

[<Test>]
let q26 () =
    Assert.AreEqual(bar [0;1;2;3], barTail [0;1;2;3])
    Assert.AreEqual(bar [10;20;30], barTail [10;20;30])
    Assert.AreEqual(bar [100;25;35], barTail [100;25;35])

[<Test>]
let q31 () =
    Assert.Throws<System.Exception>(fun () -> failDimensions (init (fun _ _ -> 0) 3 4) (init (fun _ _ -> 1) 8 9))
    
    Assert.AreEqual("Invalid matrix dimensions: m1 rows = 3, m1 columns = 4, m2 roms = 8, m2 columns = 9", (fun () -> 
        try failDimensions (init (fun _ _ -> 0) 3 4) (init (fun _ _ -> 1) 8 9)
            ""
        with
        | :? System.Exception as ex -> ex.Message
        | _ -> ""
    )())

[<Test>]
let q32 () =
    Assert.AreEqual(array2D [ [|0;1;2|]; [|1;3;5|] ], add (init (fun x y -> x + y) 2 3) (init (fun x y -> x * y) 2 3))
    Assert.AreEqual("Invalid matrix dimensions: m1 rows = 2, m1 columns = 3, m2 roms = 3, m2 columns = 2", (fun () -> 
        try add (init (fun x y -> x + y) 2 3) (init (fun x y -> x * y) 3 2) |> ignore 
            ""
        with
        | :? System.Exception as ex -> ex.Message
        | _ -> ""
    )())

[<Test>]
let q33dot () =
    Assert.AreEqual(28, dotProduct m1 m2 0 1)
    Assert.AreEqual(49, dotProduct m1 m2 1 0)


[<Test>]
let q33mult () =
    Assert.AreEqual(array2D [ [|22; 28|]; [|49; 64|]], mult m1 m2)
    Assert.AreEqual("Invalid matrix dimensions: m1 rows = 2, m1 columns = 3, m2 roms = 1, m2 columns = 9", (fun () -> 
        try mult m1 (init (fun _ _ -> 0) 1 9) |> ignore 
            ""
        with
        | :? System.Exception as ex -> ex.Message
        | _ -> ""
    )())

[<Test>]
let q34 () =
    Assert.AreEqual(array2D [[|1;2;3|]; [|4;5;6|]], parInit (fun i j -> i * 3 + j + 1) 2 3)

[<Test>]
let q41 () =
    Assert.AreEqual(([]: int32 list), emptyStack ())

[<Test>]
let q42 () =
    Assert.AreEqual(5, runStackProg [Push 5])
    Assert.AreEqual(72, runStackProg [Push 5; Push 4; Add; Push 8; Mult])
    Assert.AreEqual(114, runStackProg [Push 5; Push 4; Add; Push 8; Mult; Push 42; Add])
    
    Assert.AreEqual("empty stack", (fun () -> 
        try runStackProg [Push 5; Push 4; Add; Push 8; Mult; Mult] |> ignore 
            ""
        with
        | :? System.Exception as ex -> ex.Message
        | _ -> ""
    )())

    Assert.AreEqual("empty stack", (fun () -> 
        try runStackProg [] |> ignore 
            ""
        with
        | :? System.Exception as ex -> ex.Message
        | _ -> ""
    )())

[<Test>]
let q43 () =
    Assert.AreEqual(Some(6, [5]), push 5 >>>= push 6 >>>= pop |> evalSM)
    Assert.AreEqual(None, pop |> evalSM)
    Assert.AreEqual(None, push 5 >>>= pop >>>= pop |> evalSM)

[<Test>]
let q44 () =
    Assert.AreEqual(Some 5, [Push 5] |> runStackProg2 |> evalSM |> Option.map fst)
    Assert.AreEqual(Some 72, [Push 5; Push 4; Add; Push 8; Mult] |> runStackProg2 |> evalSM |> Option.map fst)
    Assert.AreEqual(Some 114, [Push 5; Push 4; Add; Push 8; Mult; Push 42; Add] |> runStackProg2 |> evalSM |> Option.map fst)
    Assert.AreEqual(None, [Push 5; Push 4; Add; Push 8; Mult; Mult] |> runStackProg2 |> evalSM |> Option.map fst)
    Assert.AreEqual(None, [] |> runStackProg2 |> evalSM |> Option.map fst)

open JParsec.TextParser

[<TestCase("PUSH 5\nPUSH 4\nADD\nPUSH 8\nMULT\n")>]
[<TestCase("           PUSH 5\nPUSH 4\nADD\nPUSH 8\nMULT\n")>]
[<TestCase("PUSH 5\nPUSH 4\nADD\nPUSH 8\nMULT\n              ")>]
[<TestCase("PUSH 5\nPUSH 4\nADD\nPUSH 8\nMULT            \n")>]
[<TestCase("PUSH 5\nPUSH4\nADD\nPUSH8\nMULT\n")>]
[<TestCase("PUSH             5\nPUSH          4        \nADD\nPUSH 8\nMULT\n")>]
[<TestCase(" PUSH 5 \n PUSH 4 \n ADD \n PUSH 8 \n MULT \n ")>]
[<TestCase(" PUSH 5 \n PUSH 4 \n ADD \n PUSH 8 \n MULT ")>]
[<TestCase("  PUSH         5 \n PUSH 4 \n ADD     \n         PUSH 8 \n MULT         \n ")>]
[<TestCase("PUSH 70 PUSH 2 ADD")>]
[<TestCase("PUSH 70PUSH2ADD")>]
let q45 (x) =
    let res = 
        parseStackProg x
        |> getSuccess
        |> runStackProg2
        |> evalSM
        |> Option.map fst

    Assert.AreEqual(Some 72, res)

[<TestCase("DD")>]
[<TestCase("AD")>]
[<TestCase("PUS")>]
[<TestCase("PUSH")>]
[<TestCase("PUSH d")>]
[<TestCase("MUL")>]
[<TestCase("ULT")>]
[<TestCase("ULT")>]
[<TestCase("PUS 5\nPUSH 4\nADD\nPUSH 8\nMULT\n")>]
let q45Fail (x) =
    let res = 
        parseStackProg x

    Assert.Throws<System.Exception>(fun () -> res |> getSuccess |> ignore) |> ignore