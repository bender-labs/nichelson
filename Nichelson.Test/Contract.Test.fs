namespace Nichelson.Test

open System.Numerics
open Nichelson
open Nichelson.Contract
open Nichelson.Contract.Arg
open Nichelson.Parser.Michelson

module ``Contract test`` =

    open Xunit
    open FsUnit.Xunit

    [<Fact>]
    let ``Should find nested annotation`` () =
        let parameterType =
            ContractParameters "(or (nat %amount) nat)"

        let result = parameterType.Find("%amount")

        result.Expression
        |> should equal (Parameters.load "(nat)")

    [<Fact>]
    let ``Should extract sub contract`` () =
        let contract =
            ContractParameters "(or (nat %amount) (nat %id))"

        let result = contract.Only("%amount")

        result |> should not' (equal null)

    [<Fact>]
    let ``Should instantiate single parameter unamed`` () =
        let contract = ContractParameters "(nat %amount)"

        let expression =
            contract.Instantiate("%amount", IntArg 10I)

        expression
        |> should equal (Expression.load ("10"))

    [<Fact>]
    let ``Should instantiate single parameter named`` () =
        let contract = ContractParameters "(nat %amount)"


        let expression =
            contract.Instantiate(Rec([ ("%amount", IntArg 10I) ]))

        expression
        |> should equal (Expression.load ("10"))

    [<Fact>]
    let ``Should instantiate single pair with unnamed parameters`` () =
        let contract =
            ContractParameters "(pair %ep (nat %amount) (string %id))"

        let expression =
            contract.Instantiate("%ep", Tuple [ IntArg 10I; StringArg "id" ])

        expression
        |> should equal (Expression.load (@"Pair 10 ""id"""))

    [<Fact>]
    let ``Should instantiate single pair with named parameters`` () =
        let contract =
            ContractParameters "(pair %ep (nat %amount) (string %id))"

        let expression =
            contract.Instantiate
                ("%ep",
                 Rec [ ("%amount", IntArg 10I)
                       ("%id", StringArg "id") ])

        expression
        |> should equal (Expression.load (@"Pair 10 ""id"""))

    [<Fact>]
    let ``Should instantiate nested pair with named parameters`` () =
        let contract =
            ContractParameters "(or (pair %ep (nat %amount) (string %id)) nat)"

        let expression =
            contract.Instantiate
                ("%ep",
                 Rec [ ("%amount", IntArg 10I)
                       ("%id", StringArg "id") ])


        expression
        |> should equal (Expression.load (@"(Left (Pair 10 ""id""))"))

    [<Fact>]
    let ``Should instantiate nested pair with unnamed parameters`` () =
        let contract =
            ContractParameters "(pair (pair (nat %amount) (string %id)) nat)"

        let expression =
            contract.Instantiate
                (Tuple [ IntArg 10I
                         StringArg "id"
                         IntArg 30I ])

        expression
        |> should equal (Expression.load (@"(Pair (Pair 10 ""id"") 30)"))

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
               (Expression.load
                   "0x396cf2a25842bea1de3c28f5f5c551629511cea7b1eb63078b7f6a91709132a02e3b33bb7c7f9fe9ba821aa01c3589135fd9c479fc3a037e3b4c9cb9d45f8a03")

    [<Fact>]
    let ``Should instantiate chain_id`` () =
        let contract = ContractParameters "chain_id"

        let expression =
            contract.Instantiate(StringArg "NetXm8tYqnMWky1")

        expression
        |> should equal (Expression.load "0xa8365021")

    [<Fact>]
    let ``Should instantiate or directly`` () =
        let contract = ContractParameters "(or nat string)"

        let left =
            contract.Instantiate(LeftArg(IntArg 10I))

        let right =
            contract.Instantiate(RightArg(StringArg "toto"))

        left
        |> should equal (Expression.load "(Left 10)")

        right
        |> should equal (Expression.load @"(Right ""toto"")")

    [<Fact>]
    let ``Should instantiate or by name`` () =
        let contract =
            ContractParameters "(or %action
                   (string %change_keys)
                   (nat %amount))"

        let left =
            contract.Instantiate(Rec [ ("%action", Rec [ ("%change_keys", StringArg "toto") ]) ])

        let right =
            contract.Instantiate(Rec [ ("%action", Rec [ ("%amount", IntArg 10I) ]) ])

        left
        |> should equal (Expression.load @"(Left ""toto"")")

        right
        |> should equal (Expression.load "(Right 10)")

    [<Fact>]
    let ``Should instantiate list`` () =
        let contract = ContractParameters "(list nat)"

        let result =
            contract.Instantiate(List [ IntArg 10I; IntArg 20I ])

        let expected = Expression.load "{10;20}"
        result |> should equal (expected)

    [<Fact>]
    let ``Should instantiate bytes`` () =
        let bytes = [| 255uy; 250uy |]
        let contract = ContractParameters "bytes"

        let result = contract.Instantiate(BytesArg bytes)

        let expected = Expression.load "0xfffa"
        result |> should equal (expected)

    [<Fact>]
    let ``Should instantiate bytes from string`` () =
        let contract = ContractParameters "bytes"

        let result = contract.Instantiate(StringArg "0xff")

        let expected = Expression.load "0xff"
        result |> should equal (expected)


    [<Fact>]
    let ``Should extract int value`` () =
        let contract = ContractParameters "(nat %amount)"

        let result: BigInteger =
            contract.Extract("%amount", Expression.load "100")

        result |> should equal 100I

    [<Fact>]
    let ``Should extract bytes value`` () =
        let contract = ContractParameters "(bytes %tx_id)"
        let value = Expression.load "0x50FA"

        let result: byte array = contract.Extract("%tx_id", value)

        result |> Encoder.byteToHex |> should equal "0x50fa"

    [<Fact>]
    let ``Should extract value from pair`` () =
        let contract =
            ContractParameters "(pair (nat %amount) (nat %fees))"

        let result: BigInteger =
            contract.Extract("%amount", Expression.load "(Pair 100 50)")

        result |> should equal 100I

    [<Fact>]
    let ``Should extract nested pair``() =
        let contract = ContractParameters "(pair (pair (nat %amount) (bytes %destination))
                          (pair (nat %fees) (bytes %token_id)))"
                          
        let result: BigInteger =
            contract.Extract("%fees", Expression.load "(Pair (Pair 100 0x50) (Pair 10 0x40))")
            
        result |> should equal 10I            
