module PerfTests.Main

open Fable.Core
open Fable.Import
open Thoth.Json

type ModelA = { a: string; b: string }
type ModelB = { a: int; b: int }
type ModelC = { a: int; b: ModelA list }
type ModelD = | Foo of int | Bar of string | Baz of Map<string, ModelC>
type ModelE = { a: ModelD list; b: ModelB[] }

let buildBigModel (size: int) =
    let buildA() =
        {
            ModelA.a = "Hello"
            ModelA.b = "World"
        }

    let buildB() =
        {
            ModelB.a = 42
            ModelB.b = 666
        }

    let buildC() =
        {
            ModelC.a =  42
            ModelC.b = [1..size] |> List.map (fun _ -> buildA())
        }

    let buildD() =
        [1..size]
        |> List.map (fun _ -> "Hello", buildC())
        |> Map.ofList
        |> Baz

    let buildE() =
        {
            ModelE.a = [1..size] |> List.map (fun _ -> buildD())
            ModelE.b = [|1..size|] |> Array.map (fun _ -> buildB())
        }

    buildE()


let buildDecoder() =
    Decode.Auto.generateDecoder<ModelE>() |> ignore

let measureBuildDecoder() =
    buildDecoder()
    JS.console.time("build decoder")
    for i in [0..1000] do
        buildDecoder()
    JS.console.timeEnd("build decoder")

let decoderForUseDecoder = Decode.Auto.generateDecoder<ModelE>()

let useDecoder (model: string) =
    Decode.fromString decoderForUseDecoder model |> ignore

[<Emit("require('fs').writeFileSync($0,$1)")>]
let writeFileSync (path: string) (content: string): unit = failwith "JS ONLY"

[<Emit("require('fs').readFileSync($0)")>]
let readFileSync (path: string) : string = failwith "JS ONLY"

let measureUseDecoder() =
    //let bigModel = Encode.Auto.toString(0, buildBigModel 300)
    //writeFileSync "big.json" bigModel
    let bigModel = readFileSync "big.json"
    useDecoder bigModel
    JS.console.time("use decoder")
    for i in [0..100] do
        useDecoder bigModel
    JS.console.timeEnd("use decoder")

let run () =
    measureBuildDecoder()
    measureUseDecoder()


run()
