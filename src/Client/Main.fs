open System
open System.Net
open Continuum.Core
open Continuum.Core.Types


module M = Message

type Context =
    {Socket : Tcp.Socket
     mutable InputBuffer : string}

let sendMessage socket message =
    let frame = M.ClientBase.pack message
    Tcp.writeFrameWithReconnection socket frame |> Async.RunSynchronously

let frameHandler socket frame =
    let message = M.BaseClient.unpack frame
    printfn "Received message: %A" message
    match message with
    | M.BaseClient.Connected -> sendMessage socket M.Initialize
    | M.BaseClient.Ping source -> sendMessage socket (M.Pong source)
    | _ -> ()
    async {return ()}

let rec readInput ({Socket = socket; InputBuffer = inputBuffer} as context) =
    let key = Console.ReadKey(true)
    match key.Key with
    | ConsoleKey.RightArrow ->
        sendMessage socket
            (M.Move
                {DX = 1.
                 DY = 0.})
    | ConsoleKey.LeftArrow ->
        sendMessage socket
            (M.Move
                {DX = -1.0
                 DY = 0.})
    | ConsoleKey.UpArrow ->
        sendMessage socket
            (M.Move
                {DX = 0.
                 DY = 1.})
    | ConsoleKey.DownArrow ->
        sendMessage socket
            (M.Move
                {DX = 0.
                 DY = -1.0})
    | ConsoleKey.Enter ->
        context.InputBuffer <- ""
        match inputBuffer.Split ':' with
        | [|nodeId; clientId|] ->
            sendMessage socket
                (M.Ping
                    {NodeId = NodeId nodeId
                     ObjectId = Client clientId})
            printfn ""
        | _ ->
            printfn ""
            printfn "Expected input: node-id:object-id"
    | _ ->
        context.InputBuffer <- inputBuffer + string key.KeyChar
        printf "%c" key.KeyChar
    readInput context

[<EntryPoint>]
let main =
    function
    | [|ipAddress; port|] ->
        let endpoint = IPEndPoint(IPAddress.Parse(ipAddress), Int32.Parse(port))
        let socket = Tcp.makeClient endpoint |> Async.RunSynchronously
        let inputBuffer = ""

        let context =
            {Socket = socket
             InputBuffer = inputBuffer}
        frameHandler socket
        |> Tcp.readFrames socket
        |> Async.Start
        readInput context
        0
    | _ ->
        printfn "Expected arguments: ip-address, port"
        1
