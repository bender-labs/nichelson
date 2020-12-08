namespace Nichelson.Test

open Nichelson.Contract
open Nichelson.Contract.Arg

module ``End to end`` =

    open Nichelson
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

        let parameters =
            parameterType
                .Only("%mint")
                .Instantiate(Tuple [ IntArg 10L
                                     StringArg "tz1exrEuATYhFmVSXhkCkkFzY72T75hpsthj%mint"
                                     StringArg "ethContract"
                                     StringArg "ethTxId" ])

        let encoded = Encoder.pack parameters

        (Encoder.byteToHex encoded)
        |> should
            equal
               "0x0507070707000a0a0000001a0000d3f99177aa262227a65b344416f85de34bf214206d696e740707010000000b657468436f6e7472616374010000000765746854784964"

    [<Fact>]
    let multisig () =
        let parameterType =
            ContractParameters "(pair (pair (or %action
                   (unit %change_keys)
                   (pair
                      (pair %invoke
                         (pair (nat %amount) (address %owner))
                         (pair (string %token_id) (string %tx_id)))
                      (address %target)))
                (nat %counter))
          (list %signatures (pair string signature)))"

        let r =
            parameterType.Instantiate
                (Rec [ ("%action",
                        Rec [ ("%invoke",
                               Rec [ ("%amount", IntArg 100L)
                                     ("%owner", StringArg "tz1exrEuATYhFmVSXhkCkkFzY72T75hpsthj")
                                     ("%token_id", StringArg "token")
                                     ("%tx_id", StringArg "tx") ])
                              ("%target", StringArg "KT1MUrrpFyjy8tu3udaRk74uA1Je7q6iftTZ") ])
                       ("%counter", IntArg 10L)
                       ("%signatures",
                        List [ Tuple [ StringArg "signer_id"
                                       StringArg
                                           "edsigtfKWaNLGaSC4kdXitkgS9rrcniWdR2NTuUJG8ubVXKLMyi8ZUvem2A38CXZaYdfBbSxY1gEHLkoqHZ9EBunHSq1zZz9t11" ] ]) ])

        parameterType |> should not' (equal null)

    [<Fact>]
    let ``Multisig signed payload`` () =
        let v = "(pair
                    (pair chain_id address)
                    (pair nat
                        (or
                            (unit %change_keys)
                            (pair %signer_operation
                                (or
                                    (unit %add_token)
                                    (pair %mint_token
                                        (pair (nat %amount) (address %owner))
                                        (pair (string %token_id) (string %tx_id))))
                                (address %target)))))"
        let contract = ContractParameters v

        let mint =
            Rec [ ("%amount", IntArg 100L)
                  ("%owner", StringArg "tz1S792fHX5rvs6GYP49S1U58isZkp2bNmn6")
                  ("%token_id", StringArg "tokenId")
                  ("%tx_id", StringArg "txId") ]

        let value =
            contract.Instantiate
                (Tuple [ StringArg "NetXm8tYqnMWky1"
                         StringArg "KT1Qv5H58Hsr877Rvy3DDqn1fVVAGGxCtwp2"
                         IntArg 0L
                         Rec [ ("%signer_operation",
                                   Rec [ ("%mint_token", mint)
                                         ("%target", StringArg "KT1Qv5H58Hsr877Rvy3DDqn1fVVAGGxCtwp2") ]) ] ])

        value |> should not' (equal null)
