namespace Nichelson

open Netezos.Encoding

module ChainId =
    let toBytes =
        Base58.Parse >> Array.skip 3 
