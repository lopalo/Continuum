module Continuum.BaseServer.ClientHandler

open Continuum.Core
open Continuum.Core.Types
open Continuum.BaseServer
open Continuum.BaseServer.Common

let disconnectionHandler context clientId =
    let oId = ClientId.toObjectId clientId
    context.Objects <- Map.remove oId context.Objects

let messageHandler context clientId =
    function
    | Message.BeginInitialization ->
        let entityId = ClientId.toEntityId context.NodeId clientId
        let label = Label "Foo"

        let position =
            {X = 0.0
             Y = 0.0}

        let o =
            {Label = label
             Position = position}

        context.Objects <- Map.add entityId.ObjectId o context.Objects
        let clientInitInfo : Message.ClientInitInfo =
            {Id = entityId
             Label = label
             Position = position}

        let message = Message.Initialize clientInitInfo
        Clients.sendMessage context.Clients clientId message

    | Message.Move {DX = dx; DY = dy} ->
        let oId = ClientId.toObjectId clientId
        let eId = ClientId.toEntityId context.NodeId clientId

        match Map.tryFind oId context.Objects with
        | Some o ->
            let {X = x; Y = y} = o.Position
            o.Position <-
                {X = x + dx
                 Y = y + dy}
            let message = Message.SetPosition(eId, o.Position)
            Clients.sendMessage context.Clients clientId message
        | None -> async {return ()}
