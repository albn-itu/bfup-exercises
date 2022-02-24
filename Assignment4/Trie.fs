module Trie

open System.Collections.Generic

type Dict = D of char * Dictionary<char, Dict> * bool

let emptyDictionary () : Dictionary<char, Dict> = Dictionary()

let empty () : Dict = D(' ', emptyDictionary (), false)

let rec actuallyInsert (word: char list) (D (currC, currChildren, currIsLeaf)) : Dict =
    match word with
    | [] -> D(currC, currChildren, true)
    | nextChar :: nextWord ->
        let nextNode =
            match currChildren.TryGetValue(nextChar) with
            | (true, node) -> node
            | (false, _) -> D(nextChar, emptyDictionary (), false)

        currChildren.[nextChar] <- actuallyInsert nextWord nextNode

        D(currC, currChildren, currIsLeaf)

let insert (word: string) (dict: Dict) : Dict =
    let arr = word.ToCharArray() |> Array.toList

    actuallyInsert arr dict

let rec step (c: char) (D (_, currChildren, _)) : (bool * Dict) option =
    match currChildren.TryGetValue(c) with
    | (true, node) ->
        match node with
        | D (_, _, isLeaf) -> Some(isLeaf, node)
    | (false, _) -> None

let rec actuallyLookup (word: char list) (D (currC, currChildren, currIsLeaf)) : bool =
    match word with
    | [] -> currIsLeaf
    | nextChar :: nextWord ->
        match step nextChar (D(currC, currChildren, currIsLeaf)) with
        | Some (_, nextNode) -> actuallyLookup nextWord nextNode
        | None -> false

let lookup (word: string) (dict: Dict) : bool =
    let arr = word.ToCharArray() |> Array.toList

    actuallyLookup arr dict
