namespace Nichelson.Parser.Json

open System.Numerics
open Nichelson
open Newtonsoft.Json.Linq

[<RequireQualifiedAccess>]
module Expression =

    type Func = JToken -> Expr

    let private instantiate (prop: JProperty) =
        match prop.Name with
        | "int" -> IntLiteral(BigInteger.Parse(prop.Value.Value<string>()))
        | "bytes" ->
            let hex = sprintf "0x%s" (prop.Value.Value<string>())
            BytesLiteral(Encoder.hexToBytes hex)
        | "string" ->
            StringLiteral (prop.Value.Value<string>())
        | _ -> failwith "Not supported"

    let private parsePrim (object: JObject) (f: Func) =
        let prim = object.GetValue("prim").Value<string>()
        let args = object.GetValue("args")

        match prim with
        | "Pair" -> PrimExpression.Create(D_Pair, args = (f args))
        | _ -> failwith (sprintf "Prim not supported %s" prim)

    let private parseObject (object: JObject) (f: Func) =
        if object.Count = 1 then
            object.Properties()
            |> Seq.map instantiate
            |> Seq.head
        else
            Node(parsePrim object f)

    let load (token: JToken) =
        let rec parser: Func =
            fun token ->
                match token with
                | :? JObject as object -> parseObject object parser
                | :? JArray as array ->
                    let args = array |> Seq.map parser |> Seq.toList
                    Seq args
                | _ -> failwith "Type not supported"

        parser token
