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
                  Value = "tz1exrEuATYhFmVSXhkCkkFzY72T75hpsthj" }: TezosAddress.T)
        let base58 = v |> TezosAddress.ToBase58 |> BitConverter.ToString
        base58.ToLower() |> should equal "00-00-d3-f9-91-77-aa-26-22-27-a6-5b-34-44-16-f8-5d-e3-4b-f2-14-20"