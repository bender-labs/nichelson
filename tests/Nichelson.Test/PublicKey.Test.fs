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
                
        let actual = key |> PublicKey.ToBytes |> Encoder.byteToHex
        actual |> should equal "0xe7972279962c56095d2cca63ee5f07d71d8fcf2ffba5ece4bdbc241b1ec1a9ec"


    [<Fact>]
    let ``Should create ed micheline``() =
        let key = PublicKey.FromString "edpkvQDbsaJwnY9pLCx1RWffGjNGVNrrPsoaCSAGosd6YVAjH157kV"
                
        let actual = key |> PublicKey.ForgeMicheline |> Encoder.byteToHex
        actual |> should equal "0x00e7972279962c56095d2cca63ee5f07d71d8fcf2ffba5ece4bdbc241b1ec1a9ec"
        
    [<Fact>]
    let ``Should create sp micheline``() =
        let key = PublicKey.FromString "sppk7ZdjUJTiaq8s7VsbwWxeuWL3Ljfq9EbUu9QcrvHR6c7eAE4GzjJ"
                
        let actual = key |> PublicKey.ForgeMicheline |> Encoder.byteToHex
        actual |> should equal "0x01022b5c9769181096ea1daf1288f568a4ece6858418d9a4adc0c6c4ce749307de9a"
        
    [<Fact>]
    let ``Should create p256 micheline``()=
        let key = PublicKey.FromString "p2pk67a8LF1HS5oVGoBq7buKmQoxNzsSd6KvBkSyGYKkpVzMU2PzVL1"
        
        let actual = key |> PublicKey.ForgeMicheline |> Encoder.byteToHex
        actual |> should equal "0x02038b020411ac1bf76bce28f5ec7e6a305ad161756f2dc02367eae336264a45935a"