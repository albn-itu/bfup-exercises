let readLines filePath = System.IO.File.ReadLines(filePath)

let fromFile (path: string) =
    readLines path
    |> Seq.fold (fun acc s -> Dictionary.insert s acc) (Dictionary.empty ())

printfn "Reading from file"
//let dict = fromFile "EnglishDictionary.txt"
let dict = fromFile "LargeEnglishDictionary.txt"

let queen =
    "TONIGHT IM GONNA HAVE MYSELF A REAL GOOD TIME I FEEL ALIVE AND THE WORLD ILL TURN IT INSIDE OUT YEAH IM FLOATING AROUND IN ECSTASY SO DONT STOP ME NOW CAUSE IM HAVING A GOOD TIME"

printfn "Checking Queen"
Array.iter (fun s -> printf "Lookup %s\t\t%b\n" s (Dictionary.lookup s dict)) (queen.Split ' ')

printfn "\n\nMary Poppins says: %A" (Dictionary.lookup "SUPERCALIFRAGALISTICEXPIALIDOCIOUS" dict)
