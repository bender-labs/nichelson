module Nichelson.Test.``Json Parser test``

open FsUnit.Xunit
open Newtonsoft.Json.Linq
open Xunit
open Nichelson.Parser.Json
open Nichelson

[<Fact>]
let ``Should parse nat`` () =
    let token = JToken.Parse("""{"int":"500"}""")

    let expr = Expression.load token

    expr |> should equal (IntLiteral 500I)

[<Fact>]
let ``Should parse bytes`` () =
    let token =
        JToken.Parse("""{"bytes":"850adb2175bfc9c1d5d56cf948203116b976dff3"}""")

    let expr = Expression.load token

    expr
    |> should equal (BytesLiteral(Encoder.hexToBytes "0x850adb2175bfc9c1d5d56cf948203116b976dff3"))


[<Fact>]
let ``Should parse string`` () =
    let token = JToken.Parse("""{"string":"hello"}""")

    let expr = Expression.load token

    expr |> should equal (StringLiteral "hello")

[<Fact>]
let ``Should parse pair`` () =
    let token =
        JToken.Parse("""{"prim":"Pair", "args":[{"int":"5"},{"bytes":"850a"}]}""")

    let expr = Expression.load token

    let args =
        Seq [ IntLiteral 5I
              BytesLiteral(Encoder.hexToBytes "0x850a") ]

    expr
    |> should equal (Node(PrimExpression.Create(D_Pair, args = args)))