open System
open Continuum.Core
open Continuum.Core.Types
open Continuum.BaseServer

[<EntryPoint>]
let main =
    function
    | [|nodeId; ipAddress; port|] ->
        let ipAddress = Net.IPAddress.Parse(ipAddress)
        let port = Int32.Parse(port)
        let clients = Clients.makeClients()

        let context : Common.Context =
            {NodeId = NodeId nodeId
             Clients = clients
             Objects = Map.empty}
        ClientHandler.disconnectionHandler context
        |> Clients.setDisconnectionHandler clients
        ClientHandler.messageHandler context
        |> Clients.setMessageHandler clients
        Clients.socketHandler clients
        |> Tcp.startServer ipAddress port
        |> Async.RunSynchronously
        0
    | _ ->
        printfn "Expected arguments: node-id, ip-address, port"
        1
