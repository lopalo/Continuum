module Continuum.Core.Message

module U = Continuum.Core.Util

open Continuum.Core.Types

type Cbor = PeterO.Cbor.CBORObject

type ClientBase =
    | BeginInitialization
    | Move of Speed

module ClientBase =
    let toCbor msg =
        let m = Cbor.NewMap()

        let tag =
            match msg with
            | BeginInitialization -> "BeginInitialization"
            | Move _ -> "Move"
        m.Add("tag", tag) |> ignore
        match msg with
        | BeginInitialization -> m
        | Move s -> m.Add("speed", Speed.toCbor s)

    let fromCbor o =
        match (U.getField o "tag").AsString() with
        | "BeginInitialization" ->
            BeginInitialization
        | "Move" ->
            Move(U.getField o "speed" |> Speed.fromCbor)
        | tag -> U.unknownTagError tag

    let pack msg = (toCbor msg).EncodeToBytes()

    let unpack bytes =
        Cbor.DecodeFromBytes(bytes) |> fromCbor


type ClientInitInfo =
    {Id : EntityId
     Label : Label
     Position : Position}

module ClientInitInfo =
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
    | Initialize of ClientInitInfo
    | SetPosition of id : EntityId * position : Position

module BaseClient =
    let toCbor msg =
        let m = Cbor.NewMap()

        let tag =
            match msg with
            | Connected -> "Connected"
            | Initialize _ -> "Initialize"
            | SetPosition _ -> "SetPosition"
        m.Add("tag", tag) |> ignore
        match msg with
        | Connected -> m
        | Initialize info -> m.Add("info", ClientInitInfo.toCbor info)
        | SetPosition(id, pos) ->
            m.Add("id", EntityId.toCbor id).Add("pos", Position.toCbor pos)

    let fromCbor o =
        match (U.getField o "tag").AsString() with
        | "Connected" -> Connected
        | "Initialize" ->
            U.getField o "info"
            |> ClientInitInfo.fromCbor
            |> Initialize
        | "SetPosition" ->
            SetPosition
                (id = (U.getField o "id" |> EntityId.fromCbor),
                 position = (U.getField o "pos" |> Position.fromCbor))
        | tag -> U.unknownTagError tag

    let pack msg = (toCbor msg).EncodeToBytes()

    let unpack bytes =
        Cbor.DecodeFromBytes(bytes) |> fromCbor
