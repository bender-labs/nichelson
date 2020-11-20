namespace Michelson.Test

open Bender.Michelson.Contract

module ``Contract test`` =

    open Bender.Michelson.Micheline
    open Xunit
    open FsUnit.Xunit
    open FParsec

    let parse str =
        match run Parameters.parse str with
        | Success (r, _, _) -> r
        | Failure (m, _, _) -> failwith m

    [<Fact>]
    let ``Should find by annotation`` () =
        let parameterType = ContractParameters "(nat %amount)"

        let result = parameterType.Find("%amount")

        result |> should equal (parse "(nat %amount)")

    [<Fact>]
    let ``Should find nested annotation`` () =
        let parameterType =
            ContractParameters "(or (nat %amount) nat)"

        let result = parameterType.Find("%amount")

        result |> should equal (parse "(nat %amount)")
           