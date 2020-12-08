namespace Nichelson.Test

open Nichelson
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
        let base58 = v |> TezosAddress.ToBytes |> Encoder.byteToHex
        base58.ToLower() |> should equal "0x0000d3f99177aa262227a65b344416f85de34bf21420"
    
    [<Fact>]
    let ``Should parse kt`` () =
        let v =
            TezosAddress.FromString "KT1Bgn8rVGKEDh7sGT2mUBXPHd7XWqibrMxD"

        v
        |> should
            equal
               ({ Type = KT1
                  Value = "KT1Bgn8rVGKEDh7sGT2mUBXPHd7XWqibrMxD"
                  EntryPoint = None }: TezosAddress.T)
        let base58 = v |> TezosAddress.ToBytes |> Encoder.byteToHex
        base58.ToLower() |> should equal "0x01220b596ec28379091378e9ba5e4c95fb8f5b243500"
        
    [<Fact>]
    let ``Should include ep``() =
        let v =
            TezosAddress.FromString "tz1exrEuATYhFmVSXhkCkkFzY72T75hpsthj%mint"
        let base58 = v |> TezosAddress.ToBytes |> Encoder.byteToHex
        base58.ToLower() |> should equal "0x0000d3f99177aa262227a65b344416f85de34bf214206d696e74"

    [<Fact>]
    let ``Should include ep in KT address``() =
        let v =
            TezosAddress.FromString "KT1Bgn8rVGKEDh7sGT2mUBXPHd7XWqibrMxD%mint"
        let base58 = v |> TezosAddress.ToBytes |> Encoder.byteToHex
        base58.ToLower() |> should equal "0x01220b596ec28379091378e9ba5e4c95fb8f5b2435006d696e74"