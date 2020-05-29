open System
open Continuum.Core
open Continuum.Core.Types


let sendMessage socket message =
    let frame = Message.ClientBase.pack message
    Tcp.writeFrameWithReconnection socket frame |> Async.RunSynchronously

let frameHandler socket frame =
    let message = Message.BaseClient.unpack frame
    printfn "Received message: %A" message
    match message with
    | Message.Connected ->
        sendMessage socket Message.BeginInitialization
    | _ -> ()
    async {return ()}

let rec readInput socket =
    let key = Console.ReadKey(true)
    match key.Key with
    | ConsoleKey.RightArrow ->
        sendMessage socket
            (Message.Move
                {DX = 1.
                 DY = 0.})
    | ConsoleKey.Enter ->
        printfn ""
    | _ -> ()
    readInput socket

[<EntryPoint>]
let main =
    function
    | [|ipAddress; port|] ->
        let ipAddress = Net.IPAddress.Parse(ipAddress)
        let port = Int32.Parse(port)
        let socket =
            Tcp.connectClient ipAddress port |> Async.RunSynchronously
        frameHandler socket
        |> Tcp.readFrames socket
        |> Async.Start
        readInput socket
        0
    | _ ->
        printfn "Expected arguments: ip-address, port"
        1
