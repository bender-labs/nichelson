namespace Nichelson

open System
open System.Reflection.Metadata
open System.Text
open System.Text.RegularExpressions
open Netezos.Encoding
open Nichelson

type AddressType =
    | KT1
    | TZ1
    | TZ2
    | TZ3

[<RequireQualifiedAccess>]
module TezosAddress =

    type T =
        { Type: AddressType
          Value: string
          EntryPoint: string option }

    let private prefixAsString =
        [ (KT1, "KT1")
          (TZ1, "tz1")
          (TZ2, "tz2")
          (TZ3, "tz3") ]
        |> Map.ofList

    let private base58Tags =
        [ (KT1, [| 1uy |])
          (TZ1, [| 0uy; 0uy |])
          (TZ2, [| 0uy; 1uy |])
          (TZ3, [| 0uy; 2uy |]) ]
        |> Map.ofList

    let private withoutPrefix (v: string) = v.Substring(3)

    let private (|ValidAddress|_|) input =
        let pattern = "(KT1|tz[1-3])([^%]*)%?(.*)"
        let m = Regex.Match(input, pattern)

        if (m.Success) then
            let prefix =
                match m.Groups.[1].Value with
                | "KT1" -> KT1
                | "tz1" -> TZ1
                | "tz2" -> TZ2
                | "tz3" -> TZ3
                | _ -> failwith "Unknown address type"

            Some(prefix, m.Groups.[1].Value + m.Groups.[2].Value, m.Groups.[3].Value)
        else
            None

    let FromStringUnsafe (str: string) =
        match str with
        | ValidAddress (prefix, value, ep) ->
            { Type = prefix
              Value = value
              EntryPoint = if String.IsNullOrEmpty ep then None else Some ep }
        | _ -> failwith (sprintf "Invalid address %s" str)

    let FromString (str: string) =
        try
            Ok(FromStringUnsafe str)
        with ex -> Error ex.Message

    let FromBytesUnsafe (value: byte array) =
        match value |> Array.toList with
        | h :: _ when h = 1uy ->
            { Type = KT1
              Value = Base58.Convert(value.[1..value.Length - 2], [| 2uy; 90uy; 121uy |])
              EntryPoint = None }
        | first :: (second :: _) when first = 0uy && second = 0uy ->
            { Type = TZ1
              Value = Base58.Convert(value.[2..], [| 6uy; 161uy; 159uy |])
              EntryPoint = None }
        | first :: (second :: _) when first = 0uy && second = 1uy ->
            { Type = TZ2
              Value = Base58.Convert(value.[2..], [| 6uy; 161uy; 161uy |])
              EntryPoint = None }
        | first :: (second :: _) when first = 0uy && second = 2uy ->
            { Type = TZ3
              Value = Base58.Convert(value.[2..], [| 6uy; 161uy; 164uy |])
              EntryPoint = None }
        | _ -> failwith (sprintf "Invalid address %s" (Encoder.byteToHex value))

    let FromBytes (value: byte array) =
        try
            Ok(FromBytesUnsafe value)
        with ex -> Error ex.Message

    let Value ({ Value = v }: T) = v

    let ToBytes (addr: T) =
        let full = Base58.Parse(addr.Value)
        let prefix = base58Tags.[addr.Type]
        let withoutPrefix = Array.zeroCreate (full.Length - 3)
        Buffer.BlockCopy(full, 3, withoutPrefix, 0, full.Length - 3)

        let ep =
            match addr.EntryPoint with
            | Some v -> Encoding.UTF8.GetBytes(v)
            | None -> [||]

        match addr.Type with
        | KT1 ->
            Array.concat [ prefix
                           withoutPrefix
                           [| 0uy |]
                           ep ]
        | _ ->
            Array.concat [ prefix
                           withoutPrefix
                           ep ]
