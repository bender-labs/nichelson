namespace Bender.Michelson.Contract

open System
open Bender.Michelson.Micheline
open FParsec

(*
    (pair nat nat)

    -> [1 ; 2]

    -> (Pair 1 2)

    --

    (list nat)
    -> [[1; 2; 3; 5]]


    (or (pair nat nat) nat)
    -> (Left (pair nat nat))
    -> 1 ; 2
    [left 1 2]

    parameters.extract(None).instanciate("mint", ["token_id" "mon_token" "amount" 10]);

    let exp = instanciate (params, ["mint"], [ "token_id" "mon_token" "amount" 10 ])
    // instanciate (params, [Left Right], [10])
    let packed = pack exp
    let payload = sign packed
*)

type Path = Path of string

module Path =
    let value (Path p) = p

type Values =
    | Map of Map<string, Object>
    | List of Object list

type Find = PrimExpression -> Path -> PrimExpression

type Instanciate = PrimExpression -> Path option -> Values -> Expr

type ContractParameters(michelson) =
    let typeExpression =
        let p = run Parameters.parse michelson

        match p with
        | Success (v, _, _) -> v
        | Failure (m, _, _) -> failwith m

    let location =
        let buildAnnotations =
            (fun (s: Map<string, PrimExpression>) t ->
                match t with
                | Node v ->
                    v.Annotations
                    |> List.fold (fun (m: Map<string, PrimExpression>) a -> m.Add(a, v)) s
                | _ -> s)

        Expr.fold buildAnnotations Map.empty (Node typeExpression)

    member this.Find(annotation) = location.[annotation]


[<AutoOpen>]
module Parameters =

    let find: Find =
        fun expression path ->
            let pathStr = Path.value path


            let rec folder =
                Expr.foldNodeOrSeq
                    (fun s n -> if n.Annotations |> List.contains pathStr then Some n else folder s n.Args)
                    (fun s n ->
                        n
                        |> List.fold (fun state e ->
                            match state with
                            | Some _ -> state
                            | _ -> folder s e) s)


            match folder None (Node expression) with
            | Some r -> r
            | None -> failwith "Not found"
