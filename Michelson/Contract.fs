namespace Bender.Michelson.Contract

open System
open System.Collections.Generic
open Bender.Michelson.Micheline

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

type Direction =
    | Left
    | Right

type Path = Path of string



type Values =
    | Dict of Dictionary<string, Object>
    | List of Object list

type Find = PrimExpression -> Path -> PrimExpression

type Instanciate = PrimExpression -> Path option -> Values -> Expr



[<AutoOpen>]
module Parameters =

    let _pathValue (Path p) = p

    let _nodeMatch (n) (Path path) =
        if n.Annotations |> List.contains path then Some n else None

    let _foldNodeOrSeq fNode fSeq expr =
        match expr with
        | Node n -> fNode n
        | Seq s -> fSeq s
        | _ -> None

    let find: Find =
        fun expression path ->
            let pathStr = _pathValue path

      
            let rec folder =
                _foldNodeOrSeq (fun n ->
                    if n.Annotations |> List.contains pathStr then
                        Some n
                    else
                        match n.Args with
                        | Some args -> folder args
                        | None -> None)
                    (List.fold (fun state e ->
                        match state with
                        | Some _ -> state
                        | _ -> folder e) (None: Option<PrimExpression>))


            match folder (Node expression) with
            | Some r -> r
            | None -> failwith "Not found"
