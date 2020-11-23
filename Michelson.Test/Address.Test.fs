namespace Michelson.Test

open System
open Bender.Michelson
open Xunit

module ``Address test`` =

    open FsUnit.Xunit

    [<Fact>]
    let ``Should parse tz1`` () =
        let v =
            TezosAddress.FromString "tz1exrEuATYhFmVSXhkCkkFzY72T75hpsthj"

        v
        |> should
            equal
               ({ Type = TZ1
                  Value = "tz1exrEuATYhFmVSXhkCkkFzY72T75hpsthj"
                  EntryPoint = None }: TezosAddress.T)
        let base58 = v |> TezosAddress.ToBase58 |> Encoder.byteToHex
        base58.ToLower() |> should equal "0x0000d3f99177aa262227a65b344416f85de34bf21420"
        
    [<Fact>]
    let ``Should include ep``() =
        let v =
            TezosAddress.FromString "tz1exrEuATYhFmVSXhkCkkFzY72T75hpsthj%mint"
        let base58 = v |> TezosAddress.ToBase58 |> Encoder.byteToHex
        base58.ToLower() |> should equal "0x0000d3f99177aa262227a65b344416f85de34bf214206d696e74"

