namespace Continuum.BaseServer

open Continuum.Core

open Continuum.Core.Types

type ClientId = private | ClientId of string

module ClientId =
    let toObjectId (ClientId id) = Client id

    let toEntityId nodeId clientId =
        {NodeId = nodeId
         ObjectId = toObjectId clientId}

    let NewId() = Util.generateBase64Guid() |> ClientId


type Clients =
    private {mutable MessageHandler : ClientId -> Message.ClientBase -> Async<unit>
             mutable DisconnectionHandler : ClientId -> unit
             mutable Sockets : Map<ClientId, Tcp.Socket>}


module Clients =
    let makeClients() =
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

    let socketHandler clients socket =
        let clientId = ClientId.NewId()
        let (ClientId cid) = clientId
        clients.Sockets <- Map.add clientId socket clients.Sockets
        printfn "Client '%s' connected" cid
        let cleanup() =
            clients.Sockets <- Map.remove clientId clients.Sockets
            Tcp.close socket
            printfn "Client '%s' disconnected" cid

        let rec loop() =
            async {
                match! Tcp.readFrame socket with
                | Some frame ->
                    let msg = Message.ClientBase.unpack frame
                    do! clients.MessageHandler clientId msg
                    return! loop()
                | None ->
                    return ()
            }

        async {
            try
                try
                    do! sendMessage clients clientId Message.Connected
                    do! loop()
                finally
                    cleanup()
            with error ->
                printfn "Error in client's message handler: \n%A" error
        }
        |> Async.Start
