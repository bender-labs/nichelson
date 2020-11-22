namespace Michelson.Test

open Bender.Michelson.Micheline.Parser

module ``Encoder test`` =
    
    open FsUnit.Xunit
    open Xunit
    open Bender.Michelson
    
    let pack v = Encoder.pack (Expression.fromMichelson v)
    
    [<Fact>]
    let ``Should encode int``() =
        let bytes = pack "100"
        
        (Encoder.byteToHex bytes) |> should equal "0x0500a401"

    let ``Should encode long``() =
        let bytes = pack "10147483648"
        
        (Encoder.byteToHex bytes) |> should equal "0x050080c0b2cd4b"
        
    [<Fact>]
    let ``Should encode string``() =
        let bytes = pack @"""test"""
        
        (Encoder.byteToHex bytes) |> should equal "0x05010000000474657374"
    
    [<Fact>]
    let ``Should encode pair``() =
        let bytes = pack "(Pair 1 2)"
        
        (Encoder.byteToHex bytes) |> should equal "0x05070700010002"
        
    [<Fact>]
    let ``Should encode nested pair``() =
        let bytes = pack "(Pair (Pair 1 2) 3)"
        
        (Encoder.byteToHex bytes) |> should equal "0x0507070707000100020003"
    
    [<Fact>]
    let ``Should encode Left``() =
        let bytes = pack "(Left 2)"
        
        (Encoder.byteToHex bytes) |> should equal "0x0505050002"
        
    [<Fact>]
    let ``Should encode Right``() =
        let bytes = pack "(Right 2)"
        
        (Encoder.byteToHex bytes) |> should equal "0x0505080002"
        
    [<Fact>]
    let ``Should encode string pair``() =
        let bytes = pack @"(Pair 100 ""tz1exrEuATYhFmVSXhkCkkFzY72T75hpsthj"")"
        
        (Encoder.byteToHex bytes) |> should equal "0x05070700a4010100000024747a31657872457541545968466d565358686b436b6b467a59373254373568707374686a"