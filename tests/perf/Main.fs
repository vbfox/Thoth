module PerfTests.Main

open Thoth.Json

let run () =
    let decoded = Decode.fromString (Decode.array Decode.int) "[1, 2, 3]"
    printfn "It's ALIVE %A" decoded

run()
