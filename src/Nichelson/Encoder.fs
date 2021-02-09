[<RequireQualifiedAccess>]
module Nichelson.Encoder


open System
open System.Collections.Generic
open System.Numerics
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

let private bigintToDigits b source =
    let rec loop (b : int) num digits =
        let (quotient, remainder) = bigint.DivRem(num, bigint b)
        match quotient with
        | zero when zero = 0I -> int remainder :: digits
        | _ -> loop b quotient (int remainder :: digits)
    loop b source []

let private digitsToString source =
    source
    |> List.map (fun (x : int) -> x.ToString("X").ToLowerInvariant())
    |> String.concat ""

let private forgeInt (value: bigint) =
    let mutable binary = value |> bigint.Abs |> bigintToDigits 2 |> digitsToString

    let pad =
        if ((binary.Length - 6) % 7 = 0) then binary.Length
        else if (binary.Length > 6) then binary.Length + 7 - (binary.Length - 6) % 7
        else 6

    binary <- binary.PadLeft(pad, '0')

    let septets = List<string>()

    for i in 0 .. (pad / 7) do
        septets.Add(binary.Substring(7 * i, Math.Min(7, pad - 7 * i)))

    septets.Reverse()
    septets.[0] <- (if value >= 0I then "0" else "1") + septets.[0]

    let res = List<byte>()

    for i in 0 .. (septets.Count - 1) do
        let prefix =
            if i = septets.Count - 1 then "0" else "1"

        res.Add(Convert.ToByte(prefix + septets.[i], 2))

    res.ToArray()


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
            acc.Add(0x00uy)
            acc.AddRange(forgeInt i)
            acc
        | StringLiteral s ->
            acc.Add(0x01uy)
            acc.AddRange(encodeString s)
            acc
        | BytesLiteral b ->
            acc.Add(0x0Auy)
            acc.AddRange(encodeArray b)
            acc
        | Seq elements ->
            acc.Add(0x02uy)
            let arrayBytes = List<byte>()

            elements
            |> Seq.iter (fun x -> loop (arrayBytes) x |> ignore)

            let bytes = arrayBytes |> Seq.toArray |> encodeArray
            acc.AddRange(bytes)
            acc

        | t -> failwith (sprintf "Unsupported %s" (t.ToString()))

    (loop res expr).ToArray()

let byteToHex =
    Array.map (fun (x: byte) -> String.Format("{0:x2}", x))
    >> String.concat String.Empty
    >> (+) "0x"


let hexToBytes str =
    str
    |> Seq.skip 2
    |> Seq.windowed 2
    |> Seq.mapi (fun i j -> (i, j))
    |> Seq.filter (fun (i, _) -> i % 2 = 0)
    |> Seq.map (fun (_, j) -> Byte.Parse(String(j), System.Globalization.NumberStyles.AllowHexSpecifier))
    |> Array.ofSeq
