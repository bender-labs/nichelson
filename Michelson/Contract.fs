namespace Bender.Michelson.Contract

open System
open Bender.Michelson.Micheline
open Bender.Michelson.Micheline.Expr

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

type Entrypoint(path: Expr, name: string) =
    member this.toParams() = ()

type ContractParameters(typeExpression) =

    let entryPoints =
        let rec lookup (s: {| Locations : Map<string, Prim list>; Path: Prim list |})(t: Expr) :{| Locations : Map<string, Prim list>; Path: Prim list |} =
            match t with
            | AnnotedOr (node, (left, right)) ->
                let updatedLocations = s.Locations.Add(node.Annotations.Head, s.Path |> List.rev)
                let newState = lookup {|Locations = updatedLocations ; Path = D_Left :: s.Path |} left
                lookup {| Locations = newState.Locations ; Path = D_Right :: s.Path |} right
            | Or (_, (left,right)) ->
                let newState = lookup {|s with Path = D_Left :: s.Path |} left
                lookup {| Locations = newState.Locations ; Path = D_Right :: s.Path |} right
            | AnnotedNode(node) ->
                let updatedLocations = s.Locations.Add(node.Annotations.Head, s.Path |> List.rev)
                {| s with Locations = updatedLocations |}
            | _ -> s
        
        (lookup {|Locations = Map.empty ; Path = []|} (Node typeExpression)).Locations    


    let location =
        let buildAnnotations (s: Map<string, PrimExpression>) t =
            match t with
            | Node v ->
                v.Annotations
                |> List.fold (fun (m: Map<string, PrimExpression>) a -> m.Add(a, v)) s
            | _ -> s

        fold buildAnnotations Map.empty (Node typeExpression)

    new(michelson) = ContractParameters(Parameters.fromMichelson michelson)

    member this.Find(annotation) = location.[annotation]
    
    member this.Path(annotation) = entryPoints.[annotation]

