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

        result.Expression |> should equal (parse "(nat %amount)")

    [<Fact>]
    let ``Should find nested annotation`` () =
        let parameterType =
            ContractParameters "(or (nat %amount) nat)"

        let result = parameterType.Find("%amount")

        result.Expression |> should equal (parse "(nat %amount)")
           
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
        
        result.Expression |> should equal (parse "string %remove_token")