namespace Continuum.Core

open System.Net
open Serilog
open Continuum.Core
open Continuum.Core.Types

type RemoteNodes<'Message> =
    private {MessagePacker : 'Message -> byte []
             Sockets : Map<NodeId, Tcp.Socket>}

type NodeEndPoint =
    {Id : NodeId
     EndPoint : IPEndPoint}

module Node =
    let socketHandler frameHandler socket =
        let remAddr, remPort = Tcp.remoteEndPoint socket
        Log.Information("Node connected: {0}:{1}", remAddr, remPort)
        let cleanup() =
            Tcp.close socket
            Log.Information("Node disconnected: {0}:{1}", remAddr, remPort)

        let rec loop() =
            async {
                match! Tcp.readFrame socket with
                | Some frame ->
                    do! frameHandler frame
                    return! loop()
                | None -> return ()
            }

        async {
            try
                try
                    do! loop()
                finally
                    cleanup()
            with error ->
                Log.Error("Error in node's message handler: \n%{0}", error)
        }

    let makeRemoteNodes messagePacker nodeEndpoints =
        async {
            let! nodeSockets = Seq.map (fun nep ->
                                   async {
                                       let! client = Tcp.makeClient nep.EndPoint
                                       return (nep.Id, client)}) nodeEndpoints
                               |> Async.Parallel
            let sockets =
                Array.fold
                    (fun s (nodeId, socket) -> Map.add nodeId socket s)
                    Map.empty nodeSockets
            return {MessagePacker = messagePacker
                    Sockets = sockets}
        }

    let sendMessage remoteNodes nodeId message =
        match Map.tryFind nodeId remoteNodes.Sockets with
        | Some socket ->
            remoteNodes.MessagePacker message
            |> Tcp.writeFrameWithReconnection socket
        | None -> async {return ()}
