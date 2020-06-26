open System.Net
open Serilog
open Continuum.Core
open Continuum.Core.Types
open Continuum.BaseServer

let configureLogger (config : Config.Root) =
    let loggerConfig = LoggerConfiguration()
    ignore (loggerConfig.WriteTo.Console())
    ignore
        (if config.Debug then
            loggerConfig.MinimumLevel.Debug()
         else
             loggerConfig.MinimumLevel.Information())
    Log.Logger <- loggerConfig.CreateLogger()


[<EntryPoint>]
let main =
    function
    | [|configPath; nodeId|] ->
        let config = Config.Load(configPath)
        configureLogger config
        let nodeConfig =
            config.Topology.BaseNodes |> Seq.find (fun c -> c.Id = nodeId)
        let ipAddress = IPAddress.Parse(nodeConfig.IpAddress)
        let clientPort = nodeConfig.ClientPort
        let basePort = nodeConfig.BasePort
        let clients = Clients.make nodeConfig.Capacity

        let remoteBaseEndpoints =
            config.Topology.BaseNodes
            |> Seq.filter (fun c -> c.Id <> nodeId)
            |> Seq.map (fun c ->
                let nid = NodeId c.Id
                let ipAddr = IPAddress.Parse(c.IpAddress)
                let endpoint = IPEndPoint(ipAddr, c.BasePort)
                {Id = nid
                 EndPoint = endpoint})

        let remoteBaseNodes =
            Node.makeRemoteNodes Message.BaseBase.pack remoteBaseEndpoints
            |> Async.RunSynchronously

        let context : Common.Context =
            {NodeId = NodeId nodeId
             RemoteBaseNodes = remoteBaseNodes
             Clients = clients
             Objects = Map.empty}

        ClientHandler.disconnectionHandler context
        |> Clients.setDisconnectionHandler clients

        ClientHandler.messageHandler context
        |> Clients.setMessageHandler clients


        let baseServer =
            BaseHandler.frameHandler context
            |> Node.socketHandler
            |> Tcp.startServer (IPEndPoint(ipAddress, basePort))

        let clientServer =
            Clients.socketHandler clients
            |> Tcp.startServer (IPEndPoint(ipAddress, clientPort))

        let computations =
            async {
                let! child = Async.StartChild baseServer
                let! child' = Async.StartChild clientServer
                do! child
                do! child'
                return 0
            }
        Async.RunSynchronously computations
    | _ ->
        printfn "Expected arguments: config-path, node-id"
        1
