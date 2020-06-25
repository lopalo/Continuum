namespace Continuum.BaseServer

open Serilog
open Continuum.Core
open Continuum.Core.Types

type ClientId = private | ClientId of string

module ClientId =
    let toObjectId (ClientId id) = Client id

    let fromObjectId =
        function
        | (Client id) -> Some(ClientId id)
        | Object _ -> None

    let toEntityId nodeId clientId =
        {NodeId = nodeId
         ObjectId = toObjectId clientId}

    let fromEntityId {ObjectId = oid} = fromObjectId oid

    let NewId() = Util.generateBase64Guid() |> ClientId


type Clients =
    private {mutable MessageHandler : ClientId -> Message.ClientBase -> Async<unit>
             mutable DisconnectionHandler : ClientId -> unit
             mutable Sockets : Map<ClientId, Tcp.Socket>}


module Clients =
    let make() =
        let dummyDisconnectionHandler _clientId = ()
        let dummyMessageHandler _clientId _message = async {return ()}
        {DisconnectionHandler = dummyDisconnectionHandler
         MessageHandler = dummyMessageHandler
         Sockets = Map.empty}

    let setDisconnectionHandler clients disconnectionHandler =
        clients.DisconnectionHandler <- disconnectionHandler

    let setMessageHandler clients messageHandler =
        clients.MessageHandler <- messageHandler

    let sendMessage clients clientId message =
        match Map.tryFind clientId clients.Sockets with
        | Some socket ->
            Message.BaseClient.pack message |> Tcp.writeFrame socket
        | None -> async {return ()}

    let broadcastMessage clients message =
        let frame = Message.BaseClient.pack message
        Map.toSeq clients.Sockets
        |> Seq.map (fun (_, socket) -> Tcp.writeFrame socket frame)
        |> Async.Parallel
        |> Async.Ignore

    let socketHandler clients socket =
        let clientId = ClientId.NewId()
        let (ClientId cid) = clientId
        lock clients
            (fun () ->
                clients.Sockets <- Map.add clientId socket clients.Sockets)
        Log.Debug("Client '{0}' connected", cid)
        let cleanup() =
            lock clients
                (fun () ->
                    clients.Sockets <- Map.remove clientId clients.Sockets)
            Tcp.close socket
            Log.Debug("Client '{0}' disconnected", cid)

        let rec loop() =
            async {
                match! Tcp.readFrame socket with
                | Some frame ->
                    let msg = Message.ClientBase.unpack frame
                    do! clients.MessageHandler clientId msg
                    return! loop()
                | None -> return ()
            }

        async {
            try
                try
                    do! sendMessage clients clientId Message.Connected
                    do! loop()
                finally
                    cleanup()
            with error ->
                Log.Error("Error in client's message handler: \n%{0}", error)
        }
