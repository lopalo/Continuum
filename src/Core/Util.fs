module Continuum.Core.Util

open System

type Cbor = PeterO.Cbor.CBORObject

let unknownTagError tag = sprintf "Unknown tag: %s" tag |> failwith

let getField (cborObject : Cbor) (fieldName : string) =
    match cborObject.[fieldName] with
    | null -> sprintf "Field '%s' doesn't exist" fieldName |> failwith
    | item -> item

let iterator (cborObject : Cbor) =
    cborObject.Values.GetEnumerator()

let next (iterator : Collections.Generic.IEnumerator<_>) =
    iterator.MoveNext() |> ignore
    iterator.Current

let generateBase64Guid() =
    Convert.ToBase64String(Guid.NewGuid().ToByteArray())
