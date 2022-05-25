module Exam.FourthQuestion

(* Question 4.1 *)

type Color = 
    | Red
    | Green
    | Blue
    | Purple
    | Orange
    | Yellow

type Shape =
    | Square
    | Circle
    | Star
    | Diamond
    | Cross


type tile = Shape * Color

let mkTile color shape =
    let color =
        match color with
        | "red" -> Red
        | "green" -> Green
        | "blue" -> Blue
        | "purple" -> Purple
        | "orange" -> Orange
        | "yellow" -> Yellow
        | _ -> failwith "Invalid color"
    
    match shape with
    | "square" -> (Square, color)
    | "circle" -> (Circle, color)
    | "star" -> (Star, color)
    | "diamond" -> (Diamond, color)
    | "cross" -> (Cross, color)
    | _ -> failwith "Invalid shape"

let getColor =
    function
    | Red -> "red"
    | Green -> "green"
    | Blue -> "blue"
    | Purple -> "purple"
    | Orange -> "orange"
    | Yellow -> "yellow"

let tileToString tile =
    match tile with
    | (Square, color) -> sprintf "%s square" (getColor color)
    | (Circle, color) -> sprintf "%s circle" (getColor color)
    | (Star, color) -> sprintf "%s star" (getColor color)
    | (Diamond, color) -> sprintf "%s diamond" (getColor color)
    | (Cross, color) -> sprintf "%s cross" (getColor color)

(* Question 4.2 *)

let validTiles tiles tile =
    let (tileS, tileC) = tile
    
    let rec aux tiles shapeIsSame =
        match tiles with
        | [] -> true
        | (tS,tC)::tiles ->
            match shapeIsSame with
            | true -> if tC = tileC || tS <> tileS then false else aux tiles shapeIsSame
            | false -> if tS = tileS || tC <> tileC then false else aux tiles shapeIsSame

    match tiles with
    | (t1S, _)::(t2S, _)::_ when t1S = t2S -> aux tiles true
    | _::_::_ -> aux tiles false
    | (tS, tC)::_ -> (tileS = tS && tileC <> tC) || (tileS <> tS && tileC = tC)  
    | [] -> true


(* Question 4.3 optional *)

type coord = Coord of int * int
type board = Board of Map<coord, tile>
type direction = Left | Right | Up | Down

let moveCoord (Coord(x, y)) d = 
    match d with
    | Left -> Coord (x - 1, y)
    | Right -> Coord (x + 1, y)
    | Up -> Coord (x, y - 1)
    | Down -> Coord (x, y + 1)

let collectTiles (Board b) c d =
    let rec aux acc coord =
        match Map.tryFind coord b with
        | Some c -> aux (c :: acc) (moveCoord coord d)
        | None -> acc

    aux [] c

let isEmpty b c =
    match Map.tryFind c b with
    | Some _ -> None
    | None -> Some c
    
let validateDirection b t (d, d1) c =
    match validTiles (collectTiles b (moveCoord c d) d @ collectTiles b (moveCoord c d1) d1) t with
    | true -> Some c
    | false -> None

let ret = Some
let bind f =
    function
    | None   -> None
    | Some x -> f x
let (>>=) x f = bind f x

(* Question 4.4 *)
let placeTile (c, t) (Board b) =
    c
    |> isEmpty b
    >>= validateDirection (Board b) t (Left, Right)
    >>= validateDirection (Board b) t (Up, Down)
    >>= (fun c -> Some (Board (Map.add c t b)))

(* Question 4.5 *)

(* You may use *either* railroad-oriented programming or computation expressions.
    You do not have to use both *)

(* Railroad-oriented programming *)
(* Computation expressions *)
type opt<'a> = 'a option
type OptBuilderClass() =
    member t.Return (x : 'a) : opt<'a> = ret x
    member t.ReturnFrom (x : opt<'a>) = x
    member t.Bind (o : opt<'a>, f : 'a -> opt<'b>) : opt<'b> = bind f o
let opt = new OptBuilderClass()

let placeTiles ts b =
    ts
    |> List.fold (fun s t -> s >>= placeTile t) (Some b)
    