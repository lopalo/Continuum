module Continuum.BaseServer.ClientHandler

open Continuum.Core
open Continuum.Core.Types
open Continuum.BaseServer
open Continuum.BaseServer.Common


module M = Message

let disconnectionHandler context clientId =
    let oId = ClientId.toObjectId clientId
    lock context
        (fun () -> context.Objects <- Map.remove oId context.Objects)

let messageHandler context clientId =
    function
    | M.ClientBase.Initialize ->
        let entityId = ClientId.toEntityId context.NodeId clientId
        let label = Label "Foo"

        let position =
            {X = 0.0
             Y = 0.0}

        let o =
            {Label = label
             Position = position}

        lock context
            (fun () ->
                context.Objects <-
                    Map.add entityId.ObjectId o context.Objects)
        let objInfo : M.ObjectInfo =
            {Id = entityId
             Label = label
             Position = position}

        let message = M.BaseClient.Initialize objInfo
        Clients.sendMessage context.Clients clientId message

    | M.Move {DX = dx; DY = dy} ->
        let oId = ClientId.toObjectId clientId
        let eId = ClientId.toEntityId context.NodeId clientId

        match Map.tryFind oId context.Objects with
        | Some o ->
            lock o (fun () ->
                let {X = x; Y = y} = o.Position
                o.Position <-
                    {X = x + dx
                     Y = y + dy})
            let message = M.SetPosition(eId, o.Position)
            Clients.broadcastMessage context.Clients message
        | None -> async {return ()}
    | M.Ping target ->
        let source = ClientId.toEntityId context.NodeId clientId
        let message = M.BaseBase.Ping(source, target)
        BaseHandler.sendMessage context target.NodeId message
    | M.Pong target ->
        let source = ClientId.toEntityId context.NodeId clientId
        let message = M.BaseBase.Pong(source, target)
        BaseHandler.sendMessage context target.NodeId message
