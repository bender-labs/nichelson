namespace Michelson.Test

open Bender.Michelson

module ``Signature test`` =
    
    open Xunit
    open FsUnit.Xunit
    
    [<Fact>]
    let ``Should decode from string``() =
        let signature = Signature.FromString "edsigtfKWaNLGaSC4kdXitkgS9rrcniWdR2NTuUJG8ubVXKLMyi8ZUvem2A38CXZaYdfBbSxY1gEHLkoqHZ9EBunHSq1zZz9t11"
        
        signature.Type |> should equal Ed25519
        signature.Value |> should equal "edsigtfKWaNLGaSC4kdXitkgS9rrcniWdR2NTuUJG8ubVXKLMyi8ZUvem2A38CXZaYdfBbSxY1gEHLkoqHZ9EBunHSq1zZz9t11"

    [<Fact>]
    let ``Should create base58``() =
        let signature = Signature.FromString "edsigtfKWaNLGaSC4kdXitkgS9rrcniWdR2NTuUJG8ubVXKLMyi8ZUvem2A38CXZaYdfBbSxY1gEHLkoqHZ9EBunHSq1zZz9t11"
        
        let actual = signature |> Signature.ToBytes |> Encoder.byteToHex
        actual |> should equal "0x396cf2a25842bea1de3c28f5f5c551629511cea7b1eb63078b7f6a91709132a02e3b33bb7c7f9fe9ba821aa01c3589135fd9c479fc3a037e3b4c9cb9d45f8a03" 
