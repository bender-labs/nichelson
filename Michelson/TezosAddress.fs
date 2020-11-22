namespace Bender.Michelson

open System


type AddressType =
    | KT1
    | TZ1
    | TZ2
    | TZ3

[<RequireQualifiedAccess>]
module TezosAddress =
    
    open Netezos.Forge.Utils

    type T = { Type: AddressType; Value: string }

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

    let FromString (str: string) =
        match str with
        | _ when str.StartsWith "KT1" ->
            { Type = KT1
              Value = str }
        | _ when str.StartsWith "tz1" ->
            { Type = TZ1
              Value = str }
        | _ when str.StartsWith "tz2" ->
            { Type = TZ2
              Value = str }
        | _ when str.StartsWith "tz3" ->
            { Type = TZ3
              Value = str }
        | _ -> failwith "Bad address format"

    let Value (addr: T) = prefixAsString.[addr.Type] + addr.Value

    let ToBase58 (addr: T) =
        let full = Base58.Parse(addr.Value)
        let prefix = base58Tags.[addr.Type]
        let withoutPrefix = Array.zeroCreate(full.Length-3);
        Buffer.BlockCopy(full, 3, withoutPrefix, 0, full.Length-3);
        match addr.Type with
        | KT1 -> Array.concat [ prefix; withoutPrefix; [| 0uy |] ]
        | _ -> Array.concat [ prefix; withoutPrefix ]
