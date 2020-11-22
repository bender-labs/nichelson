namespace Michelson.Test

open Bender.Michelson.Contract

module ``Contract test`` =

    open Bender.Michelson.Micheline
    open Xunit
    open FsUnit.Xunit

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