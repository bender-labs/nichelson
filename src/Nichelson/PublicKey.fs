namespace Nichelson

open System
open System.Text.RegularExpressions
open Netezos.Encoding

type KeyType =
    | Ed25519
    | Secp256k1
    | P256

[<RequireQualifiedAccess>]
module PublicKey =
    type T = { Type: KeyType; Value: string }

    let private prefixAsString =
        [ (Ed25519, "edpk")
          (Secp256k1, "sppk")
          (P256, "p2pk") ]
        |> Map.ofList

    let private base58Tags =
        [ (Ed25519, [| 13uy; 15uy; 37uy; 217uy |])
          (Secp256k1, [| 3uy; 254uy; 226uy; 86uy |])
          (P256, [| 3uy; 178uy; 139uy; 127uy |]) ]
        |> Map.ofList

    let private (|ValidKey|_|) input =
        let pattern = "(edpk|sppk|p2pk)(.*)"
        let m = Regex.Match(input, pattern)

        if (m.Success) then
            let prefix =
                match m.Groups.[1].Value with
                | "edpk" -> Ed25519
                | "sppk" -> Secp256k1
                | "p2pk" -> P256
                | _ -> failwith "Unknown key type"

            Some(prefix, m.Groups.[1].Value + m.Groups.[2].Value)
        else
            None

    let FromString str =
        match str with
        | ValidKey (prefix, value) -> { Type = prefix; Value = value }
        | _ -> failwith "Invalid key"


    let ToBytes (key: T) =
        let full = Base58.Parse(key.Value)
        let prefix = base58Tags.[key.Type]

        let withoutPrefix =
            Array.zeroCreate (full.Length - prefix.Length)

        Buffer.BlockCopy(full, prefix.Length, withoutPrefix, 0, full.Length - prefix.Length)
        withoutPrefix

    let ForgeMicheline (key: T) =
        let bytes = ToBytes key

        match key.Type with
        | Ed25519 -> Array.append [| 0uy |] bytes
        | Secp256k1 -> Array.append [| 1uy |] bytes
        | P256 -> Array.append [| 2uy |] bytes
