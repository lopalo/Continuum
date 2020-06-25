module Continuum.Core.Types

module U = Continuum.Core.Util

type Cbor = PeterO.Cbor.CBORObject

[<Struct>]
type NodeId = NodeId of string

module NodeId =
    let toCbor (NodeId v) = v

    let fromCbor (o : Cbor) = o.AsString() |> NodeId

[<Struct>]
type ObjectId =
    | Object of objectId : string
    | Client of clientId : string

module ObjectId =
    let toCbor msg =
        let m = Cbor.NewMap()

        let (tag, v) =
            match msg with
            | Object v -> "Object", v
            | Client v -> "Client", v
        m.Add("tag", tag).Add("val", v)

    let fromCbor o =
        let v = (U.getField o "val").AsString()
        match (U.getField o "tag").AsString() with
        | "Object" -> Object v
        | "Client" -> Client v
        | tag -> U.unknownTagError tag

[<Struct>]
type EntityId =
    {NodeId : NodeId
     ObjectId : ObjectId}

module EntityId =
    let toCbor msg =
        Cbor.NewMap().Add("nodeId", NodeId.toCbor msg.NodeId)
            .Add("objectId", ObjectId.toCbor msg.ObjectId)

    let fromCbor o =
        {NodeId = NodeId.fromCbor (U.getField o "nodeId")
         ObjectId = ObjectId.fromCbor (U.getField o "objectId")}

[<Struct>]
type Label = Label of string

module Label =
    let toCbor (Label v) = v

    let fromCbor (o : Cbor) = o.AsString() |> Label

[<Struct>]
[<StructuredFormatDisplay("({X} {Y})")>]
type Position =
    {X : float
     Y : float}

module Position =
    let toCbor msg = Cbor.NewArray().Add(msg.X).Add(msg.Y)

    let fromCbor o =
        let i = U.iterator o
        {X = (U.next i).AsDouble()
         Y = (U.next i).AsDouble()}

[<Struct>]
type Speed =
    {DX : float
     DY : float}

module Speed =
    let toCbor msg = Cbor.NewMap().Add("dx", msg.DX).Add("dy", msg.DY)

    let fromCbor o =
        {DX = (U.getField o "dx").AsDouble()
         DY = (U.getField o "dy").AsDouble()}

[<Literal>]
let private ConfigSamplePath = __SOURCE_DIRECTORY__ + "/config-sample.json"

type Config = FSharp.Data.JsonProvider<ConfigSamplePath>
