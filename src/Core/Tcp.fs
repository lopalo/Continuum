module Continuum.Core.Tcp

open System
open System.Net
open System.Net.Sockets

type NetSocket = Socket

type Socket =
    private {RemoteEndPoint : IPEndPoint
             mutable NetSocket : NetSocket
             Mutex : Threading.SemaphoreSlim}


let startServer (ipAddress : IPAddress) (port : int) socketHandler =
    async {
        let endpoint = IPEndPoint(ipAddress, port)
        let server =
            new NetSocket(AddressFamily.InterNetwork, SocketType.Stream,
                          ProtocolType.Tcp)
        server.Bind(endpoint)
        server.Listen(1024)
        printfn "Started listening on %A:%i" ipAddress port
        let rec loop() =
            async {
                let! socket = SocketTaskExtensions.AcceptAsync(server)
                              |> Async.AwaitTask
                let mutex = new Threading.SemaphoreSlim(1, 1)
                socketHandler
                    {RemoteEndPoint = socket.RemoteEndPoint :?> IPEndPoint
                     NetSocket = socket
                     Mutex = mutex}
                return! loop()
            }
        return! loop()
    }


let private connect (endpoint : IPEndPoint) =
    async {
        let socket =
            new NetSocket(AddressFamily.InterNetwork, SocketType.Stream,
                          ProtocolType.Tcp)
        try
            do! SocketTaskExtensions.ConnectAsync
                    (socket, (endpoint :> EndPoint)) |> Async.AwaitTask
            printfn "Connected to %A:%i" endpoint.Address endpoint.Port
        with :? AggregateException as e ->
            match e.GetBaseException() with
            | :? SocketException as e' ->
                match e'.ErrorCode with
                //ETIMEDOUT, ECONNREFUSED, EHOSTDOWN
                | 110
                | 111
                | 112 -> ()
                | _ -> raise e
            | _ -> raise e
        return socket
    }


let private write (socket : NetSocket) (bytes : byte []) =
    let length = bytes.Length
    let flags = SocketFlags.None

    let rec loop offset length =
        async {
            if length <= 0 then
                return ()
            else
                let segment = System.ArraySegment(bytes, offset, length)
                let! written = SocketTaskExtensions.SendAsync
                                   (socket, segment, flags) |> Async.AwaitTask
                return! loop (offset + written) (length - written)
        }
    loop 0 length

let private doWriteFrame (socket : NetSocket) (frame : byte []) =
    async {
        if socket.Connected && frame.Length > 0 then
            try
                let lengthFrame = BitConverter.GetBytes(frame.Length : Int32)
                do! write socket lengthFrame
                do! write socket frame

            with :? AggregateException as e ->
                match e.GetBaseException() with
                | :? SocketException as e' ->
                    match e'.ErrorCode with
                    //EPIPE, ECONNRESET, ENOTCONN
                    | 32
                    | 104
                    | 107 -> ()
                    | _ -> raise e
                | _ -> raise e
    }


let writeFrame socket frame =
    let mutex = socket.Mutex
    async {
        do! mutex.WaitAsync() |> Async.AwaitTask
        try
            do! doWriteFrame socket.NetSocket frame
        finally
            mutex.Release() |> ignore
    }

let private ensureConnection ({RemoteEndPoint = rep; NetSocket = s} as socket) =
    async {
        if not s.Connected then
            let! netSocket = connect rep
            socket.NetSocket <- netSocket
    }

let writeFrameWithReconnection socket frame =
    let mutex = socket.Mutex
    async {
        do! mutex.WaitAsync() |> Async.AwaitTask
        try
            do! ensureConnection socket
            do! doWriteFrame socket.NetSocket frame
        finally
            mutex.Release() |> ignore
    }

let checkConnection socket =
    writeFrameWithReconnection socket [||]

let private readExactly (socket : NetSocket) length =
    let buffer : byte [] = Array.zeroCreate length
    let flags = SocketFlags.None

    let rec loop offset length =
        async {
            if length <= 0 then
                return Some buffer
            else
                let segment = System.ArraySegment(buffer, offset, length)
                let! read = SocketTaskExtensions.ReceiveAsync
                                (socket, segment, flags) |> Async.AwaitTask
                if read = 0
                then return None
                else return! loop (offset + read) (length - read)
        }
    loop 0 length

let readFrame {NetSocket = socket} =
    async {
        match! readExactly socket 4 with
        | Some lengthFrame ->
            let length = BitConverter.ToInt32(lengthFrame, 0)
            return! readExactly socket length
        | None -> return None
    }

let readFrames socket frameHandler =
    let rec loop() =
        async {
            match! readFrame socket with
            | Some frame ->
                do! frameHandler frame
                return! loop()
            | None ->
                do! Async.Sleep(1000)
                return! loop()
        }
    loop()

let close {NetSocket = s} = s.Close()

let connectClient (ipAddress : IPAddress) (port : int) =
    async {
        let endpoint = IPEndPoint(ipAddress, port)
        let! netSocket = connect endpoint
        let mutex = new Threading.SemaphoreSlim(1, 1)
        return {RemoteEndPoint = endpoint
                NetSocket = netSocket
                Mutex = mutex}
    }
