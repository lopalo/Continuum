module Continuum.BaseServer.BaseHandler

open Continuum.Core
open Continuum.BaseServer
open Continuum.BaseServer.Common


module M = Message

let private messageHandler context =
    function
    | M.BaseBase.Ping(source, target) ->
        match ClientId.fromEntityId target with
        | Some clientId ->
            let message = M.BaseClient.Ping source
            Clients.sendMessage context.Clients clientId message
        | None -> async {return ()}
    | M.BaseBase.Pong(source, target) ->
        match ClientId.fromEntityId target with
        | Some clientId ->
            let message = M.BaseClient.Pong source
            Clients.sendMessage context.Clients clientId message
        | None -> async {return ()}


let frameHandler context frame =
    M.BaseBase.unpack frame |> messageHandler context

let sendMessage context nodeId message =
    if context.NodeId = nodeId then
        messageHandler context message
        |> Async.StartChild
        |> Async.Ignore
    else
        Node.sendMessage context.RemoteBaseNodes nodeId message
