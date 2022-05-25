module Exam.Tests.Second

open NUnit.Framework
open Exam.FourthQuestion

[<Test>]
let q41 () =
    Assert.AreEqual((Diamond, Blue), mkTile "blue" "diamond")
    Assert.AreEqual("blue diamond", tileToString (Diamond, Blue))

[<Test>]
let q42a () =
    Assert.IsFalse((validTiles [mkTile "blue" "diamond"; mkTile "green" "diamond"] (mkTile "purple" "square")))

[<Test>]
let q42 () =
    Assert.IsTrue(validTiles [ (Diamond, Blue); (Diamond, Red) ] (Diamond, Purple))
    Assert.IsFalse(validTiles [ (Diamond, Blue); (Diamond, Red) ] (Diamond, Blue))

    Assert.IsTrue(validTiles [ (Square, Purple); (Diamond, Purple) ] (Circle, Purple))
    Assert.IsFalse(validTiles [ (Square, Purple); (Diamond, Purple) ] (Square, Purple))

    // [blue diamond, green diamond] purple square
    Assert.IsFalse((validTiles [mkTile "blue" "diamond"; mkTile "green" "diamond"] (mkTile "purple" "square")))

[<Test>]
let q43 () =
    Assert.AreEqual(Coord(-1, 0), moveCoord (Coord(0, 0)) Left)
    Assert.AreEqual(Coord(1, 0), moveCoord (Coord(0, 0)) Right)
    Assert.AreEqual(Coord(0, -1), moveCoord (Coord(0, 0)) Up)
    Assert.AreEqual(Coord(0, 1), moveCoord (Coord(0, 0)) Down)


[<Test>]
let q431 () =
    let coords = [
        (Coord (1,1), (Diamond, Blue));
        (Coord (2,1), (Diamond, Red));
        (Coord (3,1), (Diamond, Purple));
        (Coord (1,2), (Circle, Blue));
        (Coord (2,2), (Circle, Red));
        (Coord (3,2), (Circle, Purple));
        (Coord (1,3), (Square, Blue));
        (Coord (2,3), (Square, Red));
        (Coord (3,3), (Square, Purple));
    ]

    let board = Board (Map.ofList coords)

    Assert.AreEqual([(Diamond, Purple); (Diamond, Red); (Diamond, Blue)], collectTiles board (Coord (1,1)) Right)
    Assert.AreEqual([(Diamond, Blue); (Diamond, Red); (Diamond, Purple)], collectTiles board (Coord (3,1)) Left)
    Assert.AreEqual([(Square, Blue); (Circle, Blue); (Diamond, Blue)], collectTiles board (Coord (1,1)) Down)
    Assert.AreEqual([(Diamond, Purple); (Circle, Purple); (Square, Purple)], collectTiles board (Coord (3,3)) Up)

[<Test>]
let q44 () =
    let coords = [
        (Coord (1,1), (Diamond, Blue));
        (Coord (2,1), (Diamond, Red));
        (Coord (3,1), (Diamond, Purple));
        (Coord (1,2), (Circle, Blue));
        (Coord (3,2), (Circle, Purple));
        (Coord (1,3), (Square, Blue));
        (Coord (2,3), (Square, Red));
        (Coord (3,3), (Square, Purple));
    ]

    let board = Board (Map.ofList coords)

    Assert.IsTrue(Option.isSome (placeTile (Coord (2,2), (Circle, Red)) board))
    Assert.IsTrue(Option.isNone (placeTile (Coord (2,2), (Circle, Blue)) board))
    Assert.IsTrue(Option.isNone (placeTile (Coord (2,2), (Diamond, Red)) board))
    Assert.IsTrue(Option.isNone (placeTile (Coord (1,1), (Diamond, Red)) board))

[<Test>]
let q45 () =
    let coords = [
        (Coord (1,1), (Diamond, Blue));
        (Coord (2,1), (Diamond, Red));
        (Coord (3,1), (Diamond, Purple));
        (Coord (1,3), (Square, Blue));
        (Coord (2,3), (Square, Red));
        (Coord (3,3), (Square, Purple));
    ]

    let board = Board (Map.ofList coords)

    let move = [
        (Coord (1,2), (Circle, Blue));
        (Coord (2,2), (Circle, Red));
        (Coord (3,2), (Circle, Purple));
    ]

    Assert.IsTrue(Option.isSome (placeTiles move board))

[<Test>]
let q45alt () =
    let coords = [
        (Coord (1,1), (Diamond, Blue));
        (Coord (2,1), (Diamond, Red));
        (Coord (3,1), (Diamond, Purple));
        (Coord (1,3), (Square, Blue));
        (Coord (2,3), (Square, Red));
        (Coord (3,3), (Square, Purple));
    ]

    let board = Board (Map.ofList coords)

    let move = [
        (Coord (1,2), (Circle, Blue));
        (Coord (2,2), (Circle, Blue));
        (Coord (3,2), (Circle, Purple));
    ]

    Assert.IsTrue(Option.isNone (placeTiles move board))

