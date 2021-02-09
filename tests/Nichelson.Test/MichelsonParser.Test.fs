namespace Nichelson.Test

open Nichelson
open Nichelson.Parser.Michelson

module ``Parser Test`` =
    open Xunit
    open FsUnit.Xunit
    open FParsec

    module ``Parameters parser test`` =

        let parse str =
            match run Parameters.parse str with
            | Success (r, _, _) -> r
            | Failure (m, _, _) -> failwith m

        type ``Simple types``() =

            [<Fact>]
            let ``Should parse primitive`` () =
                let expr = parse "(string)"

                expr
                |> should equal (PrimExpression.Create Prim.T_String)

            [<Fact>]
            let ``Should parse primitive without paren`` () =
                let expr = parse "string"

                expr
                |> should equal (PrimExpression.Create Prim.T_String)

            [<Fact>]
            let ``Should parse annotation`` () =
                let expr = parse "(string %token_id)"

                expr
                |> should equal (PrimExpression.Create(Prim.T_String, annotation = [ "%token_id" ]))

            [<Fact>]
            let ``Should parse annotation with numerical chars`` () =
                let expr = parse "(string %erc_20)"

                expr
                |> should equal (PrimExpression.Create(Prim.T_String, annotation = [ "%erc_20" ]))
            
            [<Fact>]
            let ``Should parse multiple annotations`` () =
                let expr = parse "(string %token_id %other)"

                expr
                |> should equal (PrimExpression.Create(Prim.T_String, annotation = [ "%token_id"; "%other" ]))

            [<Fact>]
            let ``Should parse signature`` () =
                let expr = parse "signature"

                expr
                |> should equal (PrimExpression.Create T_Signature)

            [<Fact>]
            let ``Should parse chain_id``() =
                let expr = parse "chain_id"

                expr
                |> should equal (PrimExpression.Create T_ChainId)
                
            [<Fact>]
            let ``Should parse bytes``() =
                let expr = parse "bytes"

                expr
                |> should equal (PrimExpression.Create T_Bytes)                

        type Pair() =

            [<Fact>]
            let ``Should parse pair`` () =
                let expr = parse "(pair string (nat))"

                let expected =
                    PrimExpression.Create
                        (Prim.T_Pair,
                         args =
                             Expr.Seq [ Node(PrimExpression.Create Prim.T_String)
                                        Node(PrimExpression.Create Prim.T_Nat) ])

                expr |> should equal expected

            [<Fact>]
            let ``Should parse pair with annotations`` () =
                let expr = parse "(pair %annot (string) (nat))"

                let expected =
                    PrimExpression.Create
                        (Prim.T_Pair,
                         annotation = [ "%annot" ],
                         args =
                             Expr.Seq [ Node(PrimExpression.Create Prim.T_String)
                                        Node(PrimExpression.Create Prim.T_Nat) ])

                expr |> should equal expected

            [<Fact>]
            let ``Should parse nested pair`` () =
                let expr =
                    parse "(pair %annot (pair (string) (nat)) (nat))"

                let expected =
                    PrimExpression.Create
                        (Prim.T_Pair,
                         annotation = [ "%annot" ],
                         args =
                             Expr.Seq [ Expr.Node
                                            (PrimExpression.Create
                                                (T_Pair,
                                                 args =
                                                     Seq [ Node(PrimExpression.Create T_String)
                                                           Node(PrimExpression.Create T_Nat) ]))
                                        Expr.Node(PrimExpression.Create Prim.T_Nat) ])

                expr |> should equal expected

        type Or() =
            [<Fact>]
            let ``Should parse or`` () =
                let expr = parse "(or (string) (nat))"

                let expected =
                    PrimExpression.Create
                        (T_Or,
                         args =
                             Expr.Seq [ Node(PrimExpression.Create T_String)
                                        Node(PrimExpression.Create T_Nat) ])

                expr |> should equal expected

            [<Fact>]
            let ``Should parse full parameters`` () =
                let expr = "(or
                                (or
                                  (or
                                     (or %assets_admin (unit %change_tokens_administrator) (unit %pause_token))
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

                let result = parse expr
                result |> should not' (equal null)

        type List() =
            [<Fact>]
            let ``Should parse list`` () =
                let expr = parse "(list nat)"

                let expected =
                    PrimExpression.Create(T_List, args = Node(PrimExpression.Create T_Nat))

                expr |> should equal expected

    module ``Expression parser test`` =
        let parse str =
            match run Expression.parse str with
            | Success (r, _, _) -> r
            | Failure (m, _, _) -> failwith m

        [<Fact>]
        let ``Should parse int`` () =
            let result = parse "32"

            result |> should equal (Expr.IntLiteral 32I)

        [<Fact>]
        let ``Should parse String`` () =
            let result = parse @"""Test"""

            result |> should equal (Expr.StringLiteral "Test")

        [<Fact>]
        let ``Should parse Pair`` () =
            let result = parse "(Pair 32 43)"

            result
            |> should
                equal
                   (Expr.Node(PrimExpression.Create(Prim.D_Pair, args = Seq [ IntLiteral 32I; IntLiteral 43I ])))

        [<Fact>]
        let ``Should parse nested Pair`` () =
            let result = parse @"(Pair (Pair 32 43) ""2"")"

            result
            |> should
                equal
                   (Expr.Node
                       (PrimExpression.Create
                           (Prim.D_Pair,
                            args =
                                Seq [ Node
                                          (PrimExpression.Create
                                              (Prim.D_Pair, args = Seq [ IntLiteral 32I; IntLiteral 43I ]))
                                      StringLiteral "2" ])))

        [<Fact>]
        let ``Should parse Left`` () =
            let result = parse "(Left 42)"

            result
            |> should
                equal
                   (Expr.Node
                       (PrimExpression.Create
                           (Prim.D_Left, args = IntLiteral 42I

                           )))

        [<Fact>]
        let ``Should parse Right`` () =
            let result = parse "(Right 42)"

            result
            |> should equal (Expr.Node(PrimExpression.Create(Prim.D_Right, args = IntLiteral 42I)))

        [<Fact>]
        let ``Should parse binary`` () =
            let result =
                parse "0x050a000000160000d3f99177aa262227a65b344416f85de34bf21420"

            result
            |> should
                equal
                   (Expr.BytesLiteral(Encoder.hexToBytes "0x050a000000160000d3f99177aa262227a65b344416f85de34bf21420"))

        [<Fact>]
        let ``Should parse list`` () =
            let result = parse "{10;4}"

            result
            |> should equal (Seq [ IntLiteral 10I; IntLiteral 4I ])
