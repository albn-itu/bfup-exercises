open System
open Exam

[<EntryPoint>]
let main argv =
    calculateRPN () |> evalSM |> 
    function 
    | None -> printfn "Invalid formula"
    | _    -> ()
    0 // return an integer exit code