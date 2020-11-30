namespace Bender.Michelson

open System
open System.Text.RegularExpressions
open Netezos.Forge.Utils

type SignatureType =
    | Ed25519
    | Secp256k1
    | P256
    | Sig

[<RequireQualifiedAccess>]
module Signature =
    type T = { Type: SignatureType; Value: string }

    let private prefixAsString =
        [ (Ed25519, "edsig")
          (Secp256k1, "spsig")
          (P256, "p2sig")
          (Sig, "sig") ]
        |> Map.ofList

    let private base58Tags =
        [ (Ed25519, [| 9uy; 245uy; 205uy; 134uy; 18uy |])
          (Secp256k1, [| 13uy; 115uy; 101uy; 19uy; 63uy |])
          (P256, [| 54uy; 240uy; 44uy; 52uy |])
          (Sig, [| 4uy; 130uy; 43uy |]) ]
        |> Map.ofList

    let private (|ValidSignature|_|) input =
        let pattern = "(edsig|spsig|p2sig|sig)(.*)"
        let m = Regex.Match(input, pattern)

        if (m.Success) then
            let prefix =
                match m.Groups.[1].Value with
                | "edsig" -> Ed25519
                | "spsig" -> Secp256k1
                | "p2sig" -> P256
                | "sig" -> Sig
                | _ -> failwith "Unknown signature type type"

            Some(prefix, m.Groups.[1].Value + m.Groups.[2].Value)
        else
            None

    let FromString str =
        match str with
        | ValidSignature (prefix, value) -> { Type = prefix; Value = value }
        | _ -> failwith "Invalid Signature"


    let ToBytes (signature: T) =
        let full = Base58.Parse(signature.Value)
        let prefix = base58Tags.[signature.Type]

        let withoutPrefix =
            Array.zeroCreate
                (full.Length - prefix.Length)
        Buffer.BlockCopy(full, prefix.Length, withoutPrefix, 0, full.Length - prefix.Length)
        withoutPrefix
        
