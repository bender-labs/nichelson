namespace Michelson.Test

open Bender.Michelson.Contract

module ``Contract test`` =

    open Bender.Michelson.Micheline
    open Xunit
    open FsUnit.Xunit
    open FParsec


    [<Fact>]
    let ``Should find by annotation`` () =
        let parameterType = ContractParameters "(nat %amount)"

        let result = parameterType.Find("%amount")

        result.Expression
        |> should equal (Parameters.fromMichelson "(nat %amount)")

    [<Fact>]
    let ``Should find nested annotation`` () =
        let parameterType =
            ContractParameters "(or (nat %amount) nat)"

        let result = parameterType.Find("%amount")

        result.Expression
        |> should equal (Parameters.fromMichelson "(nat %amount)")

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
        (pair %mint
           (pair (nat %amount) (address %owner))
           (pair (string %token_id) (string %tx_id))))"

        let result = parameterType.Find "%remove_token"

        result.Expression
        |> should equal (Parameters.fromMichelson "string %remove_token")

    [<Fact>]
    let ``Should extract sub contract`` () =
        let contract =
            ContractParameters "(or (nat %amount) (nat %id))"

        let result = contract.Only("%amount")

        result.Find("%amount").Path |> should be Empty

    [<Fact>]
    let ``Should instantiate single parameter unamed`` () =
        let contract = ContractParameters "(nat %amount)"

        let expression =
            contract.Instantiate("%amount", List [ 10L ])

        expression
        |> should equal (Expression.fromMichelson ("10"))

    [<Fact>]
    let ``Should instantiate single parameter named`` () =
        let contract = ContractParameters "(nat %amount)"

        let expression =
            contract.Instantiate("%amount", Map([ ("%amount", 10L :> obj) ] |> Map.ofList))

        expression
        |> should equal (Expression.fromMichelson ("10"))

    [<Fact>]
    let ``Should instantiate single pair with unnamed parameters`` () =
        let contract =
            ContractParameters "(pair %ep (nat %amount) (string %id))"

        let expression =
            contract.Instantiate("%ep", List [ 10L; "id" ])

        expression
        |> should equal (Expression.fromMichelson (@"Pair 10 ""id"""))

    [<Fact>]
    let ``Should instantiate single pair with named parameters`` () =
        let contract =
            ContractParameters "(pair %ep (nat %amount) (string %id))"

        let expression =
            contract.Instantiate
                ("%ep",
                 Map
                     ([ ("%amount", 10L :> obj)
                        ("%id", "id" :> obj) ]
                      |> Map.ofList))

        expression
        |> should equal (Expression.fromMichelson (@"Pair 10 ""id"""))

    [<Fact>]
    let ``Should instantiate nested pair with named parameters`` () =
        let contract =
            ContractParameters "(or (pair %ep (nat %amount) (string %id)) nat)"

        let expression =
            contract.Instantiate
                ("%ep",
                 Map
                     ([ ("%amount", 10L :> obj)
                        ("%id", "id" :> obj) ]
                      |> Map.ofList))

        expression
        |> should equal (Expression.fromMichelson (@"(Left (Pair 10 ""id""))"))