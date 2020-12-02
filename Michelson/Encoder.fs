[<RequireQualifiedAccess>]
module Bender.Michelson.Encoder


open System
open System.Collections.Generic
open System.Text

let private primTags =
    [ (D_Left, 0x05uy)
      (D_Pair, 0x07uy)
      (D_Right, 0x08uy) ]
    |> Map.ofList

let private lenTags =
    [ [ (false, 3uy); (true, 4uy) ] |> Map.ofList
      [ (false, 5uy); (true, 6uy) ] |> Map.ofList
      [ (false, 7uy); (true, 8uy) ] |> Map.ofList
      [ (false, 9uy); (true, 9uy) ] |> Map.ofList ]

let private toLittleEndian b =
    if (BitConverter.IsLittleEndian) then b |> Array.rev else b

let private forgeInt (value: int64) =


    let mutable binary = Convert.ToString(Math.Abs(value), 2)

    let pad =
        if ((binary.Length - 6) % 7 = 0) then binary.Length
        else if (binary.Length > 6) then binary.Length + 7 - (binary.Length - 6) % 7
        else 6

    binary <- binary.PadLeft(pad, '0')

    let septets = List<string>()

    for i in 0 .. (pad / 7) do
        septets.Add(binary.Substring(7 * i, Math.Min(7, pad - 7 * i)))

    septets.Reverse()
    septets.[0] <- (if value >= 0L then "0" else "1") + septets.[0]

    let res = List<byte>()

    for i in 0 .. (septets.Count - 1) do
        let prefix =
            if i = septets.Count - 1 then "0" else "1"

        res.Add(Convert.ToByte(prefix + septets.[i], 2))

    res.ToArray()


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
    | c, [] ->
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
        | IntLiteral i ->
            res.Add(0x00uy)
            res.AddRange(forgeInt i)
            acc
        | StringLiteral s ->
            res.Add(0x01uy)
            res.AddRange(encodeString s)
            acc
        | BytesLiteral b ->
            res.Add(0x0Auy)
            res.AddRange(encodeArray b)
            acc
        | t -> failwith (sprintf "Unsupported %s" (t.ToString()))

    (loop res expr).ToArray()

let byteToHex bytes =
    bytes
    |> Array.map (fun (x: byte) -> String.Format("{0:x2}", x))
    |> String.concat String.Empty
    |> (fun s -> "0x" + s)


let hexToBytes str =
    str
    |> Seq.skip 2
    |> Seq.windowed 2
    |> Seq.mapi (fun i j -> (i, j))
    |> Seq.filter (fun (i, j) -> i % 2 = 0)
    |> Seq.map (fun (_, j) -> Byte.Parse(new System.String(j), System.Globalization.NumberStyles.AllowHexSpecifier))
    |> Array.ofSeq
