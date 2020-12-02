namespace Michelson.Test

open Bender.Michelson
open Bender.Michelson.Contract
open Bender.Michelson.Contract.Arg

module ``Contract test`` =

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
            contract.Instantiate("%amount", IntArg 10L)

        expression
        |> should equal (Expression.fromMichelson ("10"))

    [<Fact>]
    let ``Should instantiate single parameter named`` () =
        let contract = ContractParameters "(nat %amount)"


        let expression =
            contract.Instantiate(Rec([ ("%amount", IntArg 10L) ]))

        expression
        |> should equal (Expression.fromMichelson ("10"))

    [<Fact>]
    let ``Should instantiate single pair with unnamed parameters`` () =
        let contract =
            ContractParameters "(pair %ep (nat %amount) (string %id))"

        let expression =
            contract.Instantiate("%ep", Tuple [ IntArg 10L; StringArg "id" ])

        expression
        |> should equal (Expression.fromMichelson (@"Pair 10 ""id"""))

    [<Fact>]
    let ``Should instantiate single pair with named parameters`` () =
        let contract =
            ContractParameters "(pair %ep (nat %amount) (string %id))"

        let expression =
            contract.Instantiate
                ("%ep",
                 Rec [ ("%amount", IntArg 10L)
                       ("%id", StringArg "id") ])

        expression
        |> should equal (Expression.fromMichelson (@"Pair 10 ""id"""))

    [<Fact>]
    let ``Should instantiate nested pair with named parameters`` () =
        let contract =
            ContractParameters "(or (pair %ep (nat %amount) (string %id)) nat)"

        let expression =
            contract.Instantiate
                ("%ep",
                 Rec [ ("%amount", IntArg 10L)
                       ("%id", StringArg "id") ])


        expression
        |> should equal (Expression.fromMichelson (@"(Left (Pair 10 ""id""))"))

    [<Fact>]
    let ``Should instantiate nested pair with unnamed parameters`` () =
        let contract =
            ContractParameters "(pair (pair (nat %amount) (string %id)) nat)"

        let expression =
            contract.Instantiate
                (Tuple [ IntArg 10L
                         StringArg "id"
                         IntArg 30L ])

        expression
        |> should equal (Expression.fromMichelson (@"(Pair (Pair 10 ""id"") 30)"))

    [<Fact>]
    let ``Should instantiate signature`` () =
        let contract = ContractParameters "signature"

        let expression =
            contract.Instantiate
                (StringArg
                    "edsigtfKWaNLGaSC4kdXitkgS9rrcniWdR2NTuUJG8ubVXKLMyi8ZUvem2A38CXZaYdfBbSxY1gEHLkoqHZ9EBunHSq1zZz9t11")

        expression
        |> should
            equal
               (Expression.fromMichelson
                   "0x396cf2a25842bea1de3c28f5f5c551629511cea7b1eb63078b7f6a91709132a02e3b33bb7c7f9fe9ba821aa01c3589135fd9c479fc3a037e3b4c9cb9d45f8a03")

    [<Fact>]
    let ``Should instantiate or directly`` () =
        let contract = ContractParameters "(or nat string)"

        let expression =
            contract.Instantiate(LeftArg(IntArg 10L))

        expression
        |> should equal (Expression.fromMichelson "(Left 10)")
