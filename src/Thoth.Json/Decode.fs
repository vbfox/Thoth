
namespace Thoth.Json

[<RequireQualifiedAccess>]
module Decode =

    open System.Globalization
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.Import

    module internal Helpers =
        [<Emit("typeof $0")>]
        let jsTypeof (_ : JsonValue) : string = jsNative

        [<Emit("$0 instanceof SyntaxError")>]
        let isSyntaxError (_ : JsonValue) : bool = jsNative

        let inline getField (fieldName: string) (o: JsonValue) = o?(fieldName)
        let inline isString (o: JsonValue) : bool = o :? string

        let inline isBoolean (o: JsonValue) : bool = o :? bool

        let inline isNumber (o: JsonValue) : bool = jsTypeof o = "number"

        let inline isArray (o: JsonValue) : bool = JS.Array.isArray(o)

        [<Emit("$0 === null ? false : (Object.getPrototypeOf($0 || false) === Object.prototype)")>]
        let isObject (_ : JsonValue) : bool = jsNative

        let inline isNaN (o: JsonValue) : bool = JS.Number.isNaN(!!o)

        let inline isNullValue (o: JsonValue): bool = isNull o

        [<Emit("-2147483648 < $0 && $0 < 2147483647 && ($0 | 0) === $0")>]
        let isValidIntRange (_: JsonValue) : bool = jsNative

        [<Emit("isFinite($0) && !($0 % 1)")>]
        let isIntFinite (_: JsonValue) : bool = jsNative

        [<Emit("$0 === undefined")>]
        let isUndefined (o: JsonValue): bool = jsNative

        [<Emit("JSON.stringify($0, null, 4) + ''")>]
        let anyToString (_: JsonValue) : string = jsNative

        let inline isFunction (o: JsonValue) : bool = jsTypeof o = "function"

        let inline objectKeys (o: JsonValue) : string seq = upcast JS.Object.keys(o)
        let inline asBool (o: JsonValue): bool = unbox o
        let inline asInt (o: JsonValue): int = unbox o
        let inline asFloat (o: JsonValue): float = unbox o
        let inline asString (o: JsonValue): string = unbox o
        let inline asArray (o: JsonValue): JsonValue[] = unbox o

        let prependPath (path: string) (err: DecoderError): DecoderError =
            let (oldPath, reason) = err
            (path + oldPath, reason)

        let inline prependPathToResult<'T> (path: string) (res: Result<'T, DecoderError>) =
            res |> Result.mapError(prependPath path)

    let private genericMsg msg value newLine =
        try
            "Expecting "
                + msg
                + " but instead got:"
                + (if newLine then "\n" else " ")
                + (Helpers.anyToString value)
        with
            | _ ->
                "Expecting "
                + msg
                + " but decoder failed. Couldn't report given value due to circular structure."
                + (if newLine then "\n" else " ")

    let private errorToString (path : string, error) =
        let reason =
            match error with
            | BadPrimitive (msg, value) ->
                genericMsg msg value false
            | BadType (msg, value) ->
                genericMsg msg value true
            | BadPrimitiveExtra (msg, value, reason) ->
                genericMsg msg value false + "\nReason: " + reason
            | BadField (msg, value) ->
                genericMsg msg value true
            | BadPath (msg, value, fieldName) ->
                genericMsg msg value true + ("\nNode `" + fieldName + "` is unkown.")
            | TooSmallArray (msg, value) ->
                "Expecting " + msg + ".\n" + (Helpers.anyToString value)
            | BadOneOf messages ->
                "I run into the following problems:\n\n" + String.concat "\n" messages
            | FailMessage msg ->
                "I run into a `fail` decoder: " + msg

        match error with
        | BadOneOf _ ->
            // Don't need to show the path here because each error case will show it's own path
            reason
        | _ ->
            "Error at: `" + path + "`\n" + reason

    exception DecoderException of DecoderError

    let private unwrap (decoder : Decoder<'T>) (value : obj) : 'T =
        match decoder value with
        | Ok success ->
            success
        | Error error ->
            raise (DecoderException error)

    ///////////////
    // Runners ///
    /////////////

    let fromValue (decoder : Decoder<'T>) =
        fun value ->
            match decoder value with
            | Ok success -> Ok success
            | Error error -> Error (errorToString error)

    let fromString (decoder : Decoder<'T>) =
        fun value ->
            try
                let json = JS.JSON.parse value
                match decoder json with
                | Ok success -> Ok success
                | Error error ->
                    let finalError = error |> Helpers.prependPath "$"
                    Error (errorToString finalError)
            with
                | ex when Helpers.isSyntaxError ex ->
                    Error("Given an invalid JSON: " + ex.Message)
                | DecoderException error ->
                    errorToString error
                    |> Error

    let unsafeFromString (decoder : Decoder<'T>) =
        fun value ->
            match fromString decoder value with
            | Ok x -> x
            | Error msg -> failwith msg

    [<System.Obsolete("Please use fromValue instead")>]
    let decodeValue (decoder : Decoder<'T>) = fromValue decoder

    [<System.Obsolete("Please use fromString instead")>]
    let decodeString (decoder : Decoder<'T>) = fromString decoder

    //////////////////
    // Primitives ///
    ////////////////

    let private stringUnsafe : Decoder<string> =
        fun value ->
            Ok(Helpers.asString value)

    let string : Decoder<string> =
        fun value ->
            if Helpers.isString value then
                Ok(Helpers.asString value)
            else
                ("", BadPrimitive("a string", value)) |> Error

    let guid : Decoder<System.Guid> =
        fun value ->
            if Helpers.isString value then
                match System.Guid.TryParse (Helpers.asString value) with
                | true, x -> Ok x
                | _ -> ("", BadPrimitive("a guid", value)) |> Error
            else ("", BadPrimitive("a guid", value)) |> Error

    let int : Decoder<int> =
        fun value ->
            if Helpers.isNumber value then
                if Helpers.isValidIntRange value then
                    Ok(Helpers.asInt value)
                else
                    ("", BadPrimitiveExtra("an int", value, "Value was either too large or too small for an int")) |> Error
            elif Helpers.isString value then
                match System.Int32.TryParse (Helpers.asString value) with
                | true, x -> Ok x
                | _ -> ("", BadPrimitive("an int", value)) |> Error
            else
                ("", BadPrimitive("an int", value)) |> Error

    let int64 : Decoder<int64> =
        fun value ->
            if Helpers.isNumber value then
                Helpers.asInt value |> int64 |> Ok
            elif Helpers.isString value then
                match System.Int64.TryParse (Helpers.asString value) with
                | true, x -> Ok x
                | _ -> ("", BadPrimitive("an int64", value)) |> Error
            else ("", BadPrimitive("an int64", value)) |> Error

    let uint32 : Decoder<uint32> =
        fun value ->
            if Helpers.isNumber value then
                let x = Helpers.asFloat value
                if x >= 0. && x <= (float System.UInt32.MaxValue) then
                    Helpers.asInt value |> uint32 |> Ok
                else
                    ("", BadPrimitiveExtra("an uint32", value, "Value was either too large or too small for an uint32")) |> Error
            elif Helpers.isString value then
                match System.UInt32.TryParse (Helpers.asString value) with
                | true, x -> Ok x
                | _ -> ("", BadPrimitive("an uint32", value)) |> Error
            else ("", BadPrimitive("an uint32", value)) |> Error

    let uint64 : Decoder<uint64> =
        fun value ->
            if Helpers.isNumber value then
                let x = Helpers.asFloat value
                if x >= 0. && x <= (float System.UInt64.MaxValue) then
                    Helpers.asInt value |> uint64 |> Ok
                else
                    ("", BadPrimitiveExtra("an uint64", value, "Value was either too large or too small for an uint64")) |> Error
            elif Helpers.isString value then
                match System.UInt64.TryParse (Helpers.asString value) with
                | true, x -> Ok x
                | _ -> ("", BadPrimitive("an uint64", value)) |> Error
            else ("", BadPrimitive("an uint64", value)) |> Error

    let bigint : Decoder<bigint> =
        fun value ->
            if Helpers.isNumber value then
                Helpers.asInt value |> bigint |> Ok
            elif Helpers.isString value then
                // TODO: BigInt.TryParse has been added in Fable 2.1.4
                // Don't use it for now to support lower Fable versions
                try
                    bigint.Parse (Helpers.asString value) |> Ok
                with _ ->
                    ("", BadPrimitive("a bigint", value)) |> Error
            else
                ("", BadPrimitive("a bigint", value)) |> Error

    let bool : Decoder<bool> =
        fun value ->
            if Helpers.isBoolean value then
                Ok(Helpers.asBool value)
            else
                ("", BadPrimitive("a boolean", value)) |> Error

    let float : Decoder<float> =
        fun value ->
            if Helpers.isNumber value then
                Ok(Helpers.asFloat value)
            else
                ("", BadPrimitive("a float", value)) |> Error

    let decimal : Decoder<decimal> =
        fun value ->
            if Helpers.isNumber value then
                Helpers.asFloat value |> decimal |> Ok
            elif Helpers.isString value then
                match System.Decimal.TryParse (Helpers.asString value) with
                | true, x -> Ok x
                | _ -> ("", BadPrimitive("a decimal", value)) |> Error
            else
                ("", BadPrimitive("a decimal", value)) |> Error

    let datetime : Decoder<System.DateTime> =
        fun value ->
            if Helpers.isString value then
                match System.DateTime.TryParse (Helpers.asString value) with
                | true, x -> x.ToUniversalTime() |> Ok
                | _ -> ("", BadPrimitive("a datetime", value)) |> Error
            else
                ("", BadPrimitive("a datetime", value)) |> Error

    let datetimeOffset : Decoder<System.DateTimeOffset> =
        fun value ->
            if Helpers.isString value then
                match System.DateTimeOffset.TryParse(Helpers.asString value) with
                | true, x -> Ok x
                | _ -> ("", BadPrimitive("a datetimeoffset", value)) |> Error
            else
                ("", BadPrimitive("a datetime", value)) |> Error

    let timespan : Decoder<System.TimeSpan> =
        fun value ->
            if Helpers.isString value then
                match System.TimeSpan.TryParse(Helpers.asString value) with
                | true, x -> Ok x
                | _ -> ("", BadPrimitive("a timespan", value)) |> Error
            else
                ("", BadPrimitive("a timespan", value)) |> Error

    /////////////////////////
    // Object primitives ///
    ///////////////////////

    let field (fieldName: string) (decoder : Decoder<'value>) : Decoder<'value> =
        fun value ->
            if Helpers.isObject value then
                let fieldValue = Helpers.getField fieldName value
                match decoder fieldValue with
                | Ok _ as ok -> ok
                | Error er ->
                    if Helpers.isUndefined fieldValue then
                        Error("", BadField ("an object with a field named `" + fieldName + "`", value))
                    else
                        Error(er |> Helpers.prependPath ("." + fieldName))
            else
                Error("", BadType("an object", value))

    let at (fieldNames: string list) (decoder : Decoder<'value>) : Decoder<'value> =
        fun firstValue ->
            let pathErrorMsg () =
                "an object with path `" + (String.concat "." fieldNames) + "`"
            (("", firstValue, None), fieldNames)
            ||> List.fold (fun (curPath, curValue, res) field ->
                match res with
                | Some _ -> curPath, curValue, res
                | None ->
                    if Helpers.isNullValue curValue then
                        let res = Error(curPath, BadPath(pathErrorMsg(), firstValue, field))
                        curPath, curValue, Some res
                    elif Helpers.isObject curValue then
                        let curValue = Helpers.getField field curValue
                        curPath + "." + field, curValue, None
                    else
                        let res = Error(curPath, BadType("an object", curValue))
                        curPath, curValue, Some res)
            |> function
                | _, _, Some res -> res
                | lastPath, lastValue, None ->
                    match decoder lastValue with
                    | Ok _ as ok -> ok
                    | Error er ->
                        if Helpers.isUndefined lastValue then
                            Error(lastPath, BadPath (pathErrorMsg(), firstValue, List.tryLast fieldNames |> Option.defaultValue ""))
                        else
                            Error(er |> Helpers.prependPath lastPath)

    let index (requestedIndex: int) (decoder : Decoder<'value>) : Decoder<'value> =
        fun value ->
            if Helpers.isArray value then
                let vArray = Helpers.asArray value
                if requestedIndex < vArray.Length then
                    decoder (vArray.[requestedIndex])
                    |> Helpers.prependPathToResult (".[" + (Operators.string requestedIndex) + "]")
                else
                    let msg =
                        "a longer array. Need index `"
                            + (requestedIndex.ToString())
                            + "` but there are only `"
                            + (vArray.Length.ToString())
                            + "` entries"

                    ("", TooSmallArray(msg, value))
                    |> Error
            else
                ("", BadPrimitive("an array", value))
                |> Error

    let option (decoder : Decoder<'value>) : Decoder<'value option> =
        fun value ->
            if Helpers.isNullValue value then Ok None
            else decoder value |> Result.map Some

    let optional (fieldName : string) (decoder : Decoder<'value>) : Decoder<'value option> =
        field fieldName (option decoder)

    let optionalAt (fieldNames : string list) (decoder : Decoder<'value>) : Decoder<'value option> =
        at fieldNames (option decoder)

    //////////////////////
    // Data structure ///
    ////////////////////

    let list (decoder : Decoder<'value>) : Decoder<'value list> =
        fun value ->
            if Helpers.isArray value then
                let tokens = Helpers.asArray value
                let mutable i = tokens.Length - 1
                let mutable result = []
                let mutable error: DecoderError option = None
                while i > 0 && error.IsNone do
                    let value = tokens.[i]
                    match decoder value with
                    | Ok value -> result <- value::result
                    | Error er -> error <- Some (er |> Helpers.prependPath (".[" + (i.ToString()) + "]"))
                    i <- i - 1

                if error.IsNone then
                    Ok result
                else
                    Error error.Value
            else
                ("", BadPrimitive ("a list", value))
                |> Error

    let array (decoder : Decoder<'value>) : Decoder<'value array> =
        fun value ->
            if Helpers.isArray value then
                let mutable i = 0
                let tokens = Helpers.asArray value
                let arr = ResizeArray<'value>()
                let mutable error: DecoderError option = None
                while i < tokens.Length && error.IsNone do
                    let value = tokens.[i]
                    match decoder value with
                    | Ok value -> arr.Add(value)
                    | Error er -> error <- Some (er |> Helpers.prependPath (".[" + (i.ToString()) + "]"))
                    i <- i + 1

                if error.IsNone then
                    Ok (unbox arr)
                else
                    Error error.Value
            else
                ("", BadPrimitive ("an array", value))
                |> Error

    let keyValuePairs (decoder : Decoder<'value>) : Decoder<(string * 'value) list> =
        fun value ->
            if Helpers.isObject value then
                (Ok [], Helpers.objectKeys value) ||> Seq.fold (fun acc prop ->
                    match acc with
                    | Error _ -> acc
                    | Ok acc ->
                        match Helpers.getField prop value |> decoder with
                        | Ok value -> (prop, value)::acc |> Ok
                        | Error er -> Error er)
                |> Result.map List.rev
            else
                ("", BadPrimitive ("an object", value))
                |> Error

    //////////////////////////////
    // Inconsistent Structure ///
    ////////////////////////////

    let oneOf (decoders : Decoder<'value> list) : Decoder<'value> =
        fun value ->
            let rec runner (decoders : Decoder<'value> list) (errors : string list) =
                match decoders with
                | head::tail ->
                    match fromValue head value with
                    | Ok _ as ok -> unbox ok
                    | Error error -> runner tail (errors @ [error])
                | [] -> ("", BadOneOf errors) |> Error

            runner decoders []

    //////////////////////
    // Fancy decoding ///
    ////////////////////

    let nil (output : 'a) : Decoder<'a> =
        fun value ->
            if Helpers.isNullValue value then
                Ok output
            else
                ("", BadPrimitive("null", value)) |> Error

    let value _ v = Ok v

    let succeed (output : 'a) : Decoder<'a> =
        fun _ ->
            Ok output

    let fail (msg: string) : Decoder<'a> =
        fun _ ->
            ("", FailMessage msg) |> Error

    let andThen (cb: 'a -> Decoder<'b>) (decoder : Decoder<'a>) : Decoder<'b> =
        fun value ->
            match decoder value with
            | Error _ as error-> unbox error
            | Ok result -> cb result value

    /////////////////////
    // Map functions ///
    ///////////////////

    let map
        (ctor : 'a -> 'value)
        (d1 : Decoder<'a>) : Decoder<'value> =
        fun value ->
            match d1 value with
            | Ok v1 -> Ok (ctor v1)
            | Error er -> Error er

    let map2
        (ctor : 'a -> 'b -> 'value)
        (d1 : Decoder<'a>)
        (d2 : Decoder<'b>) : Decoder<'value> =
        fun value ->
            match d1 value, d2 value with
            | Ok v1, Ok v2 -> Ok (ctor v1 v2)
            | Error er,_ -> Error er
            | _,Error er -> Error er

    let map3
        (ctor : 'a -> 'b -> 'c -> 'value)
        (d1 : Decoder<'a>)
        (d2 : Decoder<'b>)
        (d3 : Decoder<'c>) : Decoder<'value> =
        fun value ->
            match d1 value, d2 value, d3 value with
            | Ok v1, Ok v2, Ok v3 -> Ok (ctor v1 v2 v3)
            | Error er,_,_ -> Error er
            | _,Error er,_ -> Error er
            | _,_,Error er -> Error er

    let map4
        (ctor : 'a -> 'b -> 'c -> 'd -> 'value)
        (d1 : Decoder<'a>)
        (d2 : Decoder<'b>)
        (d3 : Decoder<'c>)
        (d4 : Decoder<'d>) : Decoder<'value> =
        fun value ->
            match d1 value, d2 value, d3 value, d4 value with
            | Ok v1, Ok v2, Ok v3, Ok v4 -> Ok (ctor v1 v2 v3 v4)
            | Error er,_,_,_ -> Error er
            | _,Error er,_,_ -> Error er
            | _,_,Error er,_ -> Error er
            | _,_,_,Error er -> Error er

    let map5
        (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'value)
        (d1 : Decoder<'a>)
        (d2 : Decoder<'b>)
        (d3 : Decoder<'c>)
        (d4 : Decoder<'d>)
        (d5 : Decoder<'e>) : Decoder<'value> =
        fun value ->
            match d1 value, d2 value, d3 value, d4 value, d5 value with
            | Ok v1, Ok v2, Ok v3, Ok v4, Ok v5 -> Ok (ctor v1 v2 v3 v4 v5)
            | Error er,_,_,_,_ -> Error er
            | _,Error er,_,_,_ -> Error er
            | _,_,Error er,_,_ -> Error er
            | _,_,_,Error er,_ -> Error er
            | _,_,_,_,Error er -> Error er

    let map6
        (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'value)
        (d1 : Decoder<'a>)
        (d2 : Decoder<'b>)
        (d3 : Decoder<'c>)
        (d4 : Decoder<'d>)
        (d5 : Decoder<'e>)
        (d6 : Decoder<'f>) : Decoder<'value> =
        fun value ->
            match d1 value, d2 value, d3 value, d4 value, d5 value, d6 value with
            | Ok v1, Ok v2, Ok v3, Ok v4, Ok v5, Ok v6 -> Ok (ctor v1 v2 v3 v4 v5 v6)
            | Error er,_,_,_,_,_ -> Error er
            | _,Error er,_,_,_,_ -> Error er
            | _,_,Error er,_,_,_ -> Error er
            | _,_,_,Error er,_,_ -> Error er
            | _,_,_,_,Error er,_ -> Error er
            | _,_,_,_,_,Error er -> Error er

    let map7
        (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'value)
        (d1 : Decoder<'a>)
        (d2 : Decoder<'b>)
        (d3 : Decoder<'c>)
        (d4 : Decoder<'d>)
        (d5 : Decoder<'e>)
        (d6 : Decoder<'f>)
        (d7 : Decoder<'g>) : Decoder<'value> =
        fun value ->
            match d1 value, d2 value, d3 value, d4 value, d5 value, d6 value, d7 value with
            | Ok v1, Ok v2, Ok v3, Ok v4, Ok v5, Ok v6, Ok v7 -> Ok (ctor v1 v2 v3 v4 v5 v6 v7)
            | Error er,_,_,_,_,_,_ -> Error er
            | _,Error er,_,_,_,_,_ -> Error er
            | _,_,Error er,_,_,_,_ -> Error er
            | _,_,_,Error er,_,_,_ -> Error er
            | _,_,_,_,Error er,_,_ -> Error er
            | _,_,_,_,_,Error er,_ -> Error er
            | _,_,_,_,_,_,Error er -> Error er

    let map8
        (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'value)
        (d1 : Decoder<'a>)
        (d2 : Decoder<'b>)
        (d3 : Decoder<'c>)
        (d4 : Decoder<'d>)
        (d5 : Decoder<'e>)
        (d6 : Decoder<'f>)
        (d7 : Decoder<'g>)
        (d8 : Decoder<'h>) : Decoder<'value> =
        fun value ->
            match d1 value, d2 value, d3 value, d4 value, d5 value, d6 value, d7 value, d8 value with
            | Ok v1, Ok v2, Ok v3, Ok v4, Ok v5, Ok v6, Ok v7, Ok v8 -> Ok (ctor v1 v2 v3 v4 v5 v6 v7 v8)
            | Error er,_,_,_,_,_,_,_ -> Error er
            | _,Error er,_,_,_,_,_,_ -> Error er
            | _,_,Error er,_,_,_,_,_ -> Error er
            | _,_,_,Error er,_,_,_,_ -> Error er
            | _,_,_,_,Error er,_,_,_ -> Error er
            | _,_,_,_,_,Error er,_,_ -> Error er
            | _,_,_,_,_,_,Error er,_ -> Error er
            | _,_,_,_,_,_,_,Error er -> Error er

    let dict (decoder : Decoder<'value>) : Decoder<Map<string, 'value>> =
        map Map.ofList (keyValuePairs decoder)

    //////////////////////
    // Object builder ///
    ////////////////////

    type IRequiredGetter =
        abstract Field : string -> Decoder<'a> -> 'a
        abstract At : List<string> -> Decoder<'a> -> 'a
        abstract Raw : Decoder<'a> -> 'a

    type IOptionalGetter =
        abstract Field : string -> Decoder<'a> -> 'a option
        abstract At : List<string> -> Decoder<'a> -> 'a option
        abstract Raw : Decoder<'a> -> 'a option

    type IGetters =
        abstract Required: IRequiredGetter
        abstract Optional: IOptionalGetter

    let object (builder: IGetters -> 'value) : Decoder<'value> =
        fun v ->
            builder { new IGetters with
                member __.Required =
                    { new IRequiredGetter with
                        member __.Field (fieldName : string) (decoder : Decoder<_>) =
                            unwrap (field fieldName decoder) v
                        member __.At (fieldNames : string list) (decoder : Decoder<_>) =
                            unwrap (at fieldNames decoder) v
                        member __.Raw (decoder: Decoder<_>) =
                            unwrap decoder v }
                member __.Optional =
                    { new IOptionalGetter with
                        member __.Field (fieldName : string) (decoder : Decoder<_>) =
                            unwrap (field fieldName (option decoder)) v
                        member __.At (fieldNames : string list) (decoder : Decoder<_>) =
                            unwrap (at fieldNames (option decoder)) v
                        member __.Raw (decoder: Decoder<_>) =
                            match decoder v with
                            | Ok v -> Some v
                            | Error((_, reason) as error) ->
                                match reason with
                                | BadPrimitive(_,v)
                                | BadPrimitiveExtra(_,v,_)
                                | BadType(_,v) ->
                                    if Helpers.isNullValue v then None
                                    else raise (DecoderException error)
                                | BadField _
                                | BadPath _ -> None
                                | TooSmallArray _
                                | FailMessage _
                                | BadOneOf _ -> raise (DecoderException error) }
            } |> Ok

    ///////////////////////
    // Tuples decoders ///
    ////////////////////

    let tuple2 (decoder1: Decoder<'T1>) (decoder2: Decoder<'T2>) : Decoder<'T1 * 'T2> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                succeed (v1, v2)
            )
        )

    let tuple3 (decoder1: Decoder<'T1>)
               (decoder2: Decoder<'T2>)
               (decoder3: Decoder<'T3>) : Decoder<'T1 * 'T2 * 'T3> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                index 2 decoder3
                |> andThen (fun v3 ->
                    succeed (v1, v2, v3)
                )
            )
        )

    let tuple4 (decoder1: Decoder<'T1>)
               (decoder2: Decoder<'T2>)
               (decoder3: Decoder<'T3>)
               (decoder4: Decoder<'T4>) : Decoder<'T1 * 'T2 * 'T3 * 'T4> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                index 2 decoder3
                |> andThen (fun v3 ->
                    index 3 decoder4
                    |> andThen (fun v4 ->
                        succeed (v1, v2, v3, v4)
                    )
                )
            )
        )

    let tuple5 (decoder1: Decoder<'T1>)
               (decoder2: Decoder<'T2>)
               (decoder3: Decoder<'T3>)
               (decoder4: Decoder<'T4>)
               (decoder5: Decoder<'T5>) : Decoder<'T1 * 'T2 * 'T3 * 'T4 * 'T5> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                index 2 decoder3
                |> andThen (fun v3 ->
                    index 3 decoder4
                    |> andThen (fun v4 ->
                        index 4 decoder5
                        |> andThen (fun v5 ->
                            succeed (v1, v2, v3, v4, v5)
                        )
                    )
                )
            )
        )

    let tuple6 (decoder1: Decoder<'T1>)
               (decoder2: Decoder<'T2>)
               (decoder3: Decoder<'T3>)
               (decoder4: Decoder<'T4>)
               (decoder5: Decoder<'T5>)
               (decoder6: Decoder<'T6>) : Decoder<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                index 2 decoder3
                |> andThen (fun v3 ->
                    index 3 decoder4
                    |> andThen (fun v4 ->
                        index 4 decoder5
                        |> andThen (fun v5 ->
                            index 5 decoder6
                            |> andThen (fun v6 ->
                                succeed (v1, v2, v3, v4, v5, v6)
                            )
                        )
                    )
                )
            )
        )

    let tuple7 (decoder1: Decoder<'T1>)
               (decoder2: Decoder<'T2>)
               (decoder3: Decoder<'T3>)
               (decoder4: Decoder<'T4>)
               (decoder5: Decoder<'T5>)
               (decoder6: Decoder<'T6>)
               (decoder7: Decoder<'T7>) : Decoder<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                index 2 decoder3
                |> andThen (fun v3 ->
                    index 3 decoder4
                    |> andThen (fun v4 ->
                        index 4 decoder5
                        |> andThen (fun v5 ->
                            index 5 decoder6
                            |> andThen (fun v6 ->
                                index 6 decoder7
                                |> andThen (fun v7 ->
                                    succeed (v1, v2, v3, v4, v5, v6, v7)
                                )
                            )
                        )
                    )
                )
            )
        )

    let tuple8 (decoder1: Decoder<'T1>)
               (decoder2: Decoder<'T2>)
               (decoder3: Decoder<'T3>)
               (decoder4: Decoder<'T4>)
               (decoder5: Decoder<'T5>)
               (decoder6: Decoder<'T6>)
               (decoder7: Decoder<'T7>)
               (decoder8: Decoder<'T8>) : Decoder<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 * 'T8> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                index 2 decoder3
                |> andThen (fun v3 ->
                    index 3 decoder4
                    |> andThen (fun v4 ->
                        index 4 decoder5
                        |> andThen (fun v5 ->
                            index 5 decoder6
                            |> andThen (fun v6 ->
                                index 6 decoder7
                                |> andThen (fun v7 ->
                                    index 7 decoder8
                                    |> andThen (fun v8 ->
                                        succeed (v1, v2, v3, v4, v5, v6, v7, v8)
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )

    //////////////////
    // Reflection ///
    ////////////////

    open FSharp.Reflection

    // As generics are erased by Fable, let's just do an unsafe cast for performance
    let inline boxDecoder (d: Decoder<'T>): BoxedDecoder =
        !!d // d >> Result.map box

    let inline unboxDecoder (d: BoxedDecoder): Decoder<'T> =
        !!d // d >> Result.map unbox

    // This is used to force Fable use a generic comparer for map keys
    let private toMap<'key, 'value when 'key: comparison> (xs: ('key*'value) seq) = Map.ofSeq xs
    let private toSet<'key when 'key: comparison> (xs: 'key seq) = Set.ofSeq xs

    let private autoObject (decoderInfos: (string * BoxedDecoder)[]) (value: JsonValue) =
        if not (Helpers.isObject value) then
            ("", BadPrimitive ("an object", value)) |> Error
        else
            let mutable i = 0
            let mutable result = ResizeArray<obj>()
            let mutable error: DecoderError option = None
            while i < decoderInfos.Length && error.IsNone do
                let (name, decoder) = decoderInfos.[i]
                match field name decoder value with
                | Ok v -> result.Add(box v)
                | Error err -> error <- Some err
                i <- i + 1

            if error.IsNone then
                Ok (unbox<obj[]> result)
            else
                Error error.Value

    let private autoObject2 (keyDecoder: BoxedDecoder) (valueDecoder: BoxedDecoder) (value: JsonValue) =
        if not (Helpers.isObject value) then
            ("", BadPrimitive ("an object", value)) |> Error
        else
            let keys = Helpers.objectKeys(value) |> Array.ofSeq
            let mutable i = 0
            let mutable result = ResizeArray<obj*obj>()
            let mutable error: DecoderError option = None
            while i < keys.Length && error.IsNone do
                let name = keys.[i]
                match keyDecoder name with
                | Error er -> error <- Some er
                | Ok k ->
                    match field name valueDecoder value with
                    | Error er -> error <- Some er
                    | Ok v -> result.Add((k,v))
                i <- i + 1

            if error.IsNone then
                Ok (unbox<(obj*obj)[]> result)
            else
                Error error.Value

    let private mixedArray msg (decoders: BoxedDecoder[]) (values: JsonValue[]): Result<JsonValue [], DecoderError> =
        if decoders.Length <> values.Length then
            ("", sprintf "Expected %i %s but got %i" decoders.Length msg values.Length
            |> FailMessage) |> Error
        else
            let mutable i = 0
            let result = Array.zeroCreate<JsonValue> values.Length
            let mutable error: DecoderError option = None
            while i < values.Length && error.IsNone do
                let value = values.[i]
                let decoder = decoders.[i]

                match decoder value with
                | Ok value -> result.[i] <- value
                | Error er -> error <- Some er

                i <- i + 1

            if error.IsNone then
                Ok result
            else
                Error error.Value

    let rec private makeUnion extra isCamelCase t =
        let unionCases =
            FSharpType.GetUnionCases(t, allowAccessToPrivateRepresentation=true)
            |> Array.map (fun uci -> uci, uci.GetFields() |> Array.map (fun fi -> autoDecoder extra isCamelCase false fi.PropertyType))

        fun name (values: JsonValue[]) ->
            let unionCase = unionCases |> Array.tryFind (fun (x, _) -> x.Name = name)
            match unionCase with
            | None -> ("", FailMessage("Cannot find case " + name + " in " + t.FullName)) |> Error
            | Some (uci, decoders) ->
                if values.Length = 0 then
                    FSharpValue.MakeUnion(uci, [||], allowAccessToPrivateRepresentation=true) |> Ok
                else
                    mixedArray "union fields" decoders values
                    |> Result.map (fun values -> FSharpValue.MakeUnion(uci, values, allowAccessToPrivateRepresentation=true))

    and private autoDecodeRecordsAndUnions extra (isCamelCase : bool) (isOptional : bool) (t: System.Type) : BoxedDecoder =
        if FSharpType.IsRecord(t, allowAccessToPrivateRepresentation=true) then
            let decoders =
                FSharpType.GetRecordFields(t, allowAccessToPrivateRepresentation=true)
                |> Array.map (fun fi ->
                    let name =
                        if isCamelCase then fi.Name.[..0].ToLowerInvariant() + fi.Name.[1..]
                        else fi.Name
                    name, autoDecoder extra isCamelCase false fi.PropertyType)
            fun value ->
                autoObject decoders value
                |> Result.map (fun xs -> FSharpValue.MakeRecord(t, unbox xs, allowAccessToPrivateRepresentation=true))

        elif FSharpType.IsUnion(t, allowAccessToPrivateRepresentation=true) then
            let unionMaker = makeUnion extra isCamelCase t
            fun (value: JsonValue) ->
                if Helpers.isString(value) then
                    let name = Helpers.asString value
                    unionMaker name [||]
                elif Helpers.isArray(value) then
                    let values = Helpers.asArray value
                    let name = Helpers.asString values.[0]
                    unionMaker name values.[1..]
                else ("", BadPrimitive("a string or array", value)) |> Error

        else
            if isOptional then
                // The error will only happen at runtime if the value is not null
                // See https://github.com/MangelMaxime/Thoth/pull/84#issuecomment-444837773
                boxDecoder(fun value -> Error("", BadType("an extra coder for " + t.FullName, value)))
            else
                // TODO: `failwithf "... %s" t.FullName` doesn't fail immediately in Fable, investigate
                sprintf "Cannot generate auto decoder for %s. Please pass an extra decoder." t.FullName |> failwith

    and private autoDecoder (extra: ExtraCoders) isCamelCase (isOptional : bool) (t: System.Type) : BoxedDecoder =
      let fullname = t.FullName
      match Map.tryFind fullname extra with
      | Some(_,decoder) -> decoder
      | None ->
        if t.IsArray then
            let decoder = t.GetElementType() |> autoDecoder extra isCamelCase false
            array decoder |> boxDecoder
        elif t.IsGenericType then
            if FSharpType.IsTuple(t) then
                let decoders = FSharpType.GetTupleElements(t) |> Array.map (autoDecoder extra isCamelCase false)
                fun value ->
                    if Helpers.isArray value then
                        mixedArray "tuple elements" decoders (Helpers.asArray value)
                        |> Result.map (fun xs -> FSharpValue.MakeTuple(xs, t))
                    else ("", BadPrimitive ("an array", value)) |> Error
            else
                let fullname = t.GetGenericTypeDefinition().FullName
                if fullname = typedefof<obj option>.FullName then
                    t.GenericTypeArguments.[0] |> (autoDecoder extra isCamelCase true) |> option |> boxDecoder
                elif fullname = typedefof<obj list>.FullName then
                    t.GenericTypeArguments.[0] |> (autoDecoder extra isCamelCase false) |> list |> boxDecoder
                elif fullname = typedefof< Map<string, obj> >.FullName then
                    let keyDecoder = stringUnsafe |> boxDecoder
                    let valueDecoder = t.GenericTypeArguments.[1] |> autoDecoder extra isCamelCase false
                    oneOf [
                        autoObject2 keyDecoder valueDecoder
                        array (tuple2 keyDecoder valueDecoder)
                    ] |> map (fun ar -> toMap (unbox ar) |> box)
                elif fullname = typedefof< Set<string> >.FullName then
                    let decoder = t.GenericTypeArguments.[0] |> autoDecoder extra isCamelCase false
                    fun value ->
                        match array decoder value with
                        | Error er -> Error er
                        | Ok ar -> toSet (unbox ar) |> box |> Ok
                else
                    autoDecodeRecordsAndUnions extra isCamelCase isOptional t
        else
            if fullname = typeof<bool>.FullName then
                boxDecoder bool
            elif fullname = typeof<string>.FullName then
                boxDecoder string
            elif fullname = typeof<int>.FullName then
                boxDecoder int
            elif fullname = typeof<uint32>.FullName then
                boxDecoder uint32
            elif fullname = typeof<float>.FullName then
                boxDecoder float
            // These number types require extra libraries in Fable. To prevent penalizing
            // all users, extra decoders (withInt64, etc) must be passed when they're needed.

            // elif fullname = typeof<int64>.FullName then
            //     boxDecoder int64
            // elif fullname = typeof<uint64>.FullName then
            //     boxDecoder uint64
            // elif fullname = typeof<bigint>.FullName then
            //     boxDecoder bigint
            // elif fullname = typeof<decimal>.FullName then
            //     boxDecoder decimal
            elif fullname = typeof<System.DateTime>.FullName then
                boxDecoder datetime
            elif fullname = typeof<System.DateTimeOffset>.FullName then
                boxDecoder datetimeOffset
            elif fullname = typeof<System.TimeSpan>.FullName then
                boxDecoder timespan
            elif fullname = typeof<System.Guid>.FullName then
                boxDecoder guid
            elif fullname = typeof<obj>.FullName then
                fun v -> Ok v
            else autoDecodeRecordsAndUnions extra isCamelCase isOptional t

    type Auto =
        /// ATTENTION: Use this only when other arguments (isCamelCase, extra) don't change
        static member generateDecoderCached<'T>(?isCamelCase : bool, ?extra: ExtraCoders, [<Inject>] ?resolver: ITypeResolver<'T>): Decoder<'T> =
            let t = Util.resolveType resolver
            Util.CachedDecoders.GetOrAdd(t.FullName, fun _ ->
                let isCamelCase = defaultArg isCamelCase false
                let extra = match extra with Some e -> e | None -> Map.empty
                autoDecoder extra isCamelCase false t) |> unboxDecoder

        static member generateDecoder<'T>(?isCamelCase : bool, ?extra: ExtraCoders, [<Inject>] ?resolver: ITypeResolver<'T>): Decoder<'T> =
            let isCamelCase = defaultArg isCamelCase false
            let extra = match extra with Some e -> e | None -> Map.empty
            Util.resolveType resolver |> autoDecoder extra isCamelCase false |> unboxDecoder

        static member fromString<'T>(json: string, ?isCamelCase : bool, ?extra: ExtraCoders, [<Inject>] ?resolver: ITypeResolver<'T>): Result<'T, string> =
            let decoder = Auto.generateDecoder(?isCamelCase=isCamelCase, ?extra=extra, ?resolver=resolver)
            fromString decoder json

        static member unsafeFromString<'T>(json: string, ?isCamelCase : bool, ?extra: ExtraCoders, [<Inject>] ?resolver: ITypeResolver<'T>): 'T =
            let decoder = Auto.generateDecoder(?isCamelCase=isCamelCase, ?extra=extra, ?resolver=resolver)
            match fromString decoder json with
            | Ok x -> x
            | Error msg -> failwith msg
