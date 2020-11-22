[<RequireQualifiedAccess>]
module Bender.Michelson.Encoder


open System
open System.Collections.Generic
open System.Text
open Bender.Michelson.Micheline

let private primTags = [
                          (D_Left, 0x05uy)
                          (D_Pair, 0x07uy)
                          (D_Right, 0x08uy)
                          ] |> Map.ofList

let private lenTags =
    [ [ (false, 3uy); (true, 4uy) ] |> Map.ofList
      [ (false, 5uy); (true, 6uy) ] |> Map.ofList
      [ (false, 7uy); (true, 8uy) ] |> Map.ofList
      [ (false, 9uy); (true, 9uy) ] |> Map.ofList ]

let private toLittleEndian b =
    if (BitConverter.IsLittleEndian) then b |> Array.rev else b

let private encodeInt (v: int64) =
    match v with
    | _ when (v <= int64 (Int16.MaxValue)) -> BitConverter.GetBytes(int16 (v)) |> toLittleEndian
    | _ when (v <= int64 (Int32.MaxValue)) -> BitConverter.GetBytes(int32 (v)) |> toLittleEndian
    | _ -> BitConverter.GetBytes(v) |> toLittleEndian

let private encodeArray (array: byte []) =
    let length =
        BitConverter.GetBytes(array.Length)
        |> toLittleEndian

    Array.concat [ length; array ]

let private encodeString (v: string) =
    let bytes = Encoding.UTF8.GetBytes(v)
    encodeArray bytes

let private encodePrim (acc: List<byte>) (v: PrimExpression) (loop: List<byte> -> Expr -> List<byte>) =

    match v.Args, v.Annotations with
    | Seq args, [] ->
        acc.Add(lenTags.[args.Length].[false])
        acc.Add(primTags.[v.Prim])
        args |> List.fold loop acc
    | _ as c , [] ->
        acc.Add(lenTags.[1].[false])
        acc.Add(primTags.[v.Prim])
        loop acc c
    | _, _ -> failwith "Not supported"        



let pack (expr: Expr) =
    let res = List<byte>()
    res.Add(0x05uy)

    let rec loop acc v =
        match v with
        | Node p -> encodePrim acc p loop
        | Int i ->
            res.AddRange(encodeInt i)
            acc
        | String s ->
            res.Add(0x01uy)
            res.AddRange(encodeString s)
            acc
        | _ -> failwith "Bad expression"

    (loop res expr).ToArray()

let byteToHex bytes =
    let v =
        bytes
        |> Array.map (fun (x: byte) -> System.String.Format("{0:x2}", x))
        |> String.concat System.String.Empty

    "0x" + v
