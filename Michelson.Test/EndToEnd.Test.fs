namespace Michelson.Test

open System.Text
open Bender.Michelson.Contract

module ``End to end`` =

    open Bender.Michelson
    open Xunit
    open FsUnit.Xunit

    [<Fact>]
    let ``real case`` () =
        let parameterType =
            ContractParameters "(or (or (or (or %assets_admin (unit %change_tokens_administrator) (unit %pause_token))
                (pair %burn (pair (nat %amount) (string %destination)) (string %token_id)))
            (or (or %contract_admin
                   (or (address %set_administrator) (address %set_governance))
                   (address %set_signer))
                (or %governance
                   (or (pair %add_token
                          (pair (pair (nat %decimals) (string %eth_contract))
                                (pair (string %eth_symbol) (string %name)))
                          (string %symbol))
                       (string %remove_token))
                   (nat %set_fees_ratio))))
        (pair %mint (pair (nat %amount) (address %owner))
           (pair (string %token_id) (string %tx_id))))"

        let params =
            parameterType
                .Only("%mint")
                .Instantiate("%mint",
                             List [ 10L
                                    "tz1exrEuATYhFmVSXhkCkkFzY72T75hpsthj"
                                    "ethContract"
                                    "ethTxId" ])
        let encoded = Encoder.pack params

        (Encoder.byteToHex encoded)
        |> should
            equal
               "0x0507070707000a0100000024747a31657872457541545968466d565358686b436b6b467a59373254373568707374686a0707010000000b657468436f6e7472616374010000000765746854784964"
