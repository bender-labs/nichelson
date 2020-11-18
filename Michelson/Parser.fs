[<AutoOpen>]
module Bender.Michelson.Micheline.Parser

open Bender.Michelson.Micheline
open FParsec

type private UserState = unit

let private ws = spaces

let (elements: Parser<PrimExpression, UserState>), (elementsR: Parser<PrimExpression, UserState> ref) = createParserForwardedToRef ()

let private openParen = pstring "("
let private closingParen = pstring ")"

let private annotations: Parser<Annotation, UserState> =
    let start = anyOf "%!:$&?"

    let value =
        many1Satisfy (fun c -> isLetter (c) || isAnyOf "_.%@" c)

    many
        (spaces
         >>? pipe2 start value (fun a v -> (string a) + v))

let private noArg str p =
    let v = skipString str

    v >>. annotations
    |>> (fun a ->
        { Prim = p
          Args = None
          Annotations = a })


let private nodeWithArgs str p (parser: Parser<Expr, UserState>) =
    let v = skipString str

    v
    >>. (pipe2 annotations parser (fun annot args ->
             { Prim = p
               Args = Some args
               Annotations = annot }))

let private args2 str p =
    nodeWithArgs str p (pipe2 elements elements (fun el1 el2 -> Seq [ Expr.Node el1; Expr.Node el2 ]))

let private tNat = noArg "nat" T_Nat
let private tUnit = noArg "unit" T_Nat

let private tAddress = noArg "address" T_Address

let private tString = noArg "string" T_String

let private tPair = args2 "pair" T_Pair

let private tOr = args2 "or" T_Or

let private literal =
    choice [ tPair
             tOr
             tNat
             tAddress
             tString
             tUnit ]

let private element =
    spaces >>. between openParen closingParen literal

do elementsR := element

let parseMichelineExpression = elements .>> eof