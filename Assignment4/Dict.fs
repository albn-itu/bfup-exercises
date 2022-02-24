module Dictionary

open System.Collections.Generic

type Dict = D of char * Dictionary<char, Dict> * bool

let sep = '$'

let emptyDictionary() : Dictionary<char, Dict> = Dictionary() 

let empty () : Dict = D(' ', emptyDictionary(), false)

let getNode (w: char) (dict: Dict) : Dict option =
    match dict with
    | D (_, children, _) ->
        match children.TryGetValue(w) with
        | (true, value) -> Some value
        | (false, _) -> None
      
let rec actuallyInsert (word: char list) (dict: Dict) : Dict =
    match dict with
    | D (currC, children, isLeaf) ->
        match word with
        | [] -> D(currC, children, true)
        | c :: w ->
            let node = getNode c dict

            match node with
            | Some node ->
                children.[c] <- actuallyInsert w node
                D(currC, children, isLeaf)
            | None ->
                let newNode = D(c, emptyDictionary(), false)

                children.[c] <- actuallyInsert w newNode

                D(currC, children, isLeaf)

let insert (word: string) (dict: Dict) : Dict =
    let arr = word.ToCharArray()

    let mutable newDict = dict

    for i in 1 .. arr.Length do
        let prefix, suffix = Array.splitAt i arr

        let w =
            (prefix |> Array.rev |> Array.toList)
            @ [ sep ] @ (suffix |> Array.toList)

        newDict <- actuallyInsert w newDict

    newDict

let rec step (c: char) (dict: Dict) : (bool * Dict) option =
    match getNode c dict with
    | Some node ->
        match node with
        | D (_, _, isLeaf) -> Some(isLeaf, node)
    | None -> None

let reverse (dict: Dict) : (bool * Dict) option =
    match getNode sep dict with
    | Some node ->
        match node with
        | D (_, _, isLeaf) -> Some(isLeaf, node)
    | None -> None

let rec actuallyLookup (word: char list) (dict: Dict) : bool =
    match word with
    | [] ->
        match dict with
        | D (_, _, isLeaf) -> isLeaf
    | c :: w ->
        match step c dict with
        | Some (_, node) -> actuallyLookup w node
        | None ->
            match reverse dict with
            | Some (_, node) -> actuallyLookup ([ c ] @ w) node
            | None -> false

let lookup (word: string) (dict: Dict) : bool =
    let arr = word.ToCharArray() |> Array.toList

    actuallyLookup arr dict
