namespace Nichelson.Test

open Netezos.Keys
open Nichelson

module ``Public key test`` =
    
    open Xunit
    open FsUnit.Xunit
    
    [<Fact>]
    let ``Should decode ed key from string``() =
        let key = PublicKey.FromString "edpkvQDbsaJwnY9pLCx1RWffGjNGVNrrPsoaCSAGosd6YVAjH157kV"
        
        key.Type |> should equal KeyType.Ed25519
        key.Value |> should equal "edpkvQDbsaJwnY9pLCx1RWffGjNGVNrrPsoaCSAGosd6YVAjH157kV"

    [<Fact>]
    let ``Should create ed base58``() =
        let key = PublicKey.FromString "edpkvQDbsaJwnY9pLCx1RWffGjNGVNrrPsoaCSAGosd6YVAjH157kV"
        let other = PubKey.FromBase58 "edpkvQDbsaJwnY9pLCx1RWffGjNGVNrrPsoaCSAGosd6YVAjH157kV"
                
        let actual = key |> PublicKey.ToBytes |> Encoder.byteToHex
        other.GetBytes() |> Encoder.byteToHex |> should equal "0xe7972279962c56095d2cca63ee5f07d71d8fcf2ffba5ece4bdbc241b1ec1a9ec"
        actual |> should equal "0xe7972279962c56095d2cca63ee5f07d71d8fcf2ffba5ece4bdbc241b1ec1a9ec"
