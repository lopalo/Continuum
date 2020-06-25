[<RequireQualifiedAccess>]
module Continuum.Core.Message

module U = Continuum.Core.Util

open Continuum.Core.Types

type Cbor = PeterO.Cbor.CBORObject

type ClientBase =
    | Initialize
    | Move of Speed
    | Ping of target : EntityId
    | Pong of target : EntityId

module ClientBase =
    let toCbor msg =
        let m = Cbor.NewMap()

        let tag =
            match msg with
            | Initialize -> "Initialize"
            | Move _ -> "Move"
            | Ping _ -> "Ping"
            | Pong _ -> "Pong"
        m.Add("tag", tag) |> ignore
        match msg with
        | Initialize -> m
        | Move s -> m.Add("speed", Speed.toCbor s)
        | Ping id
        | Pong id -> m.Add("id", EntityId.toCbor id)

    let fromCbor o =
        match (U.getField o "tag").AsString() with
        | "Initialize" -> Initialize
        | "Move" ->
            U.getField o "speed"
            |> Speed.fromCbor
            |> Move
        | "Ping" ->
            U.getField o "id"
            |> EntityId.fromCbor
            |> Ping
        | "Pong" ->
            U.getField o "id"
            |> EntityId.fromCbor
            |> Pong
        | tag -> U.unknownTagError tag

    let pack msg = (toCbor msg).EncodeToBytes()

    let unpack bytes = Cbor.DecodeFromBytes(bytes) |> fromCbor


type ObjectInfo =
    {Id : EntityId
     Label : Label
     Position : Position}

module ObjectInfo =
    let toCbor msg =
        Cbor.NewMap().Add("id", EntityId.toCbor msg.Id)
            .Add("label", Label.toCbor msg.Label)
            .Add("position", Position.toCbor msg.Position)

    let fromCbor o =
        {Id = U.getField o "id" |> EntityId.fromCbor
         Label = U.getField o "label" |> Label.fromCbor
         Position = U.getField o "position" |> Position.fromCbor}

type BaseClient =
    | Connected
    | Initialize of ObjectInfo
    | SetPosition of id : EntityId * position : Position
    | Ping of source : EntityId
    | Pong of source : EntityId

module BaseClient =
    let toCbor msg =
        let m = Cbor.NewMap()

        let tag =
            match msg with
            | Connected -> "Connected"
            | Initialize _ -> "Initialize"
            | SetPosition _ -> "SetPosition"
            | Ping _ -> "Ping"
            | Pong _ -> "Pong"
        m.Add("tag", tag) |> ignore
        match msg with
        | Connected -> m
        | Initialize info -> m.Add("info", ObjectInfo.toCbor info)
        | SetPosition(id, pos) ->
            m.Add("id", EntityId.toCbor id).Add("pos", Position.toCbor pos)
        | Ping source
        | Pong source -> m.Add("source", EntityId.toCbor source)

    let fromCbor o =
        match (U.getField o "tag").AsString() with
        | "Connected" -> Connected
        | "Initialize" ->
            U.getField o "info"
            |> ObjectInfo.fromCbor
            |> Initialize
        | "SetPosition" ->
            SetPosition
                (id = (U.getField o "id" |> EntityId.fromCbor),
                 position = (U.getField o "pos" |> Position.fromCbor))
        | "Ping" ->
            U.getField o "source"
            |> EntityId.fromCbor
            |> Ping
        | "Pong" ->
            U.getField o "source"
            |> EntityId.fromCbor
            |> Pong
        | tag -> U.unknownTagError tag

    let pack msg = (toCbor msg).EncodeToBytes()

    let unpack bytes = Cbor.DecodeFromBytes(bytes) |> fromCbor


type BaseBase =
    | Ping of source : EntityId * target : EntityId
    | Pong of source : EntityId * target : EntityId

module BaseBase =
    let toCbor msg =
        let m = Cbor.NewMap()

        let tag =
            match msg with
            | Ping _ -> "Ping"
            | Pong _ -> "Pong"
        m.Add("tag", tag) |> ignore
        match msg with
        | Ping(source, target)
        | Pong(source, target) ->
            m.Add("source", EntityId.toCbor source)
             .Add("target", EntityId.toCbor target)

    let fromCbor o =
        match (U.getField o "tag").AsString() with
        | "Ping" ->
            Ping
                (source = (U.getField o "source" |> EntityId.fromCbor),
                 target = (U.getField o "target" |> EntityId.fromCbor))
        | "Pong" ->
            Pong
                (source = (U.getField o "source" |> EntityId.fromCbor),
                 target = (U.getField o "target" |> EntityId.fromCbor))
        | tag -> U.unknownTagError tag

    let pack msg = (toCbor msg).EncodeToBytes()

    let unpack bytes = Cbor.DecodeFromBytes(bytes) |> fromCbor
