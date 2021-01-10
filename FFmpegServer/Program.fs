namespace FFmpegServer

open Argu

open System.Net.WebSockets
open FSharp.Control.Websockets

open System
open System.Buffers
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Threading
open System.Threading.Tasks

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging

[<AutoOpen>]
module Extensions =
    module List =
        let defaultValue value list =
            if list |> List.isEmpty
            then [ value ]
            else list

    module Seq = 
        let limit count =
            Seq.indexed >> Seq.takeWhile (fun (index, _) -> index < count) >> Seq.map snd

    type IQueryCollection with
        member this.TryGet key =
            match this.TryGetValue key with
            | (true, value) -> value |> Seq.toList
            | (false, _) -> []

    type Async with
        static member AwaitTaskWithCancellation (f : CancellationToken -> ValueTask) =
            async.Bind (Async.CancellationToken, f >> (fun vt -> vt.AsTask () |> Async.AwaitTask))

module WebSocket =
    let handle (handler : HttpContext -> WebSocket -> Async<unit>) (ctx : HttpContext) (next : unit -> Async<unit>) =
        async {
            if ctx.WebSockets.IsWebSocketRequest then
                use! ws = ctx.WebSockets.AcceptWebSocketAsync () |> Async.AwaitTask
                do! handler ctx ws
            else
                do! next ()
        }

module ProgressUI =
    type UI = MailboxProcessor<string * (int * int)>
    let createUI listeningUrl =
        MailboxProcessor.Start (fun proc -> 
            let progress = Dictionary<string, int * int> ()
            let rec loop () =
                async {
                    let! (file, p) = proc.Receive ()
                    progress.[file] <- p
                    Console.Clear ()
                    printfn "Listening on %s" listeningUrl
                    if (progress.Count > 0) then
                        let sorted = progress |> Seq.sortByDescending (fun pair -> float (fst pair.Value) / float (snd pair.Value))
                        let keyLength = progress.Keys |> Seq.map String.length |> Seq.max
                        let progressBarLength = Console.WindowWidth - keyLength - 3
                        for KeyValue (key, (f, t)) in sorted |> Seq.limit (Console.WindowHeight - 1) do
                            let progBar = String.replicate (int (float progressBarLength * float f / float t)) "#"
                            printf "%-*s [%-*s]" keyLength key progressBarLength progBar
                    return! loop ()
                }
            loop ()
        )

module FFmpeg =
    let run path args workingdir =
        // printfn "Starting FFmpeg with args: %s" args
        ProcessStartInfo ( 
            fileName = path,
            arguments = args,
            WorkingDirectory = workingdir,
            CreateNoWindow = true,
            UseShellExecute = false,
            RedirectStandardInput = true,
            RedirectStandardOutput = true,
            RedirectStandardError = true )
        |> Process.Start

    let handleSocket (ui : ProgressUI.UI) ffmpegPath ffargs outputDir (ctx : HttpContext) (ws : WebSocket) =
        let bufferPool = ArrayPool<byte>.Shared
        let bufferSize = 16 * 1024
        let query = ctx.Request.Query.TryGet
        match query "frameCount", query "name", query "ffInput", query "ffOutput" with
        | [ frameCount ], [ name ], [ input ], output when output.Length > 0 -> 
            async {
                let frameCount = Int32.Parse frameCount
                ui.Post (name, (0, frameCount))
                let ffargs = sprintf "%s-hide_banner %s -i pipe: %s" ffargs input (output |> String.concat " ")
                use ffmpeg = run ffmpegPath ffargs outputDir
                // ffmpeg.OutputDataReceived.Add (fun output ->
                //     printfn "FFmpeg out: %s" output.Data
                // )
                ffmpeg.BeginOutputReadLine ()
                ffmpeg.ErrorDataReceived.Add (fun error ->
                    // printfn "FFmpeg error: %s" error.Data
                    if error.Data <> null && error.Data.StartsWith "frame=" then
                        let frame = error.Data.Substring("frame=".Length).TrimStart().Split(' ').[0]
                        ui.Post (name, (Int32.Parse frame, frameCount))
                        let bytes = Text.Encoding.UTF8.GetBytes frame
                        WebSocket.asyncSend ws (ArraySegment bytes) WebSocketMessageType.Text true |> Async.Start
                )
                ffmpeg.BeginErrorReadLine ()
                let bufferArray = bufferPool.Rent bufferSize
                let buffer = ArraySegment bufferArray
                while ws.State = WebSocketState.Open do
                    let! result = WebSocket.asyncReceive ws buffer
                    if result.MessageType = WebSocketMessageType.Close then
                        do! WebSocket.asyncCloseOutput ws WebSocketCloseStatus.NormalClosure "Received close message" |> Async.Ignore
                    let data = ReadOnlyMemory(bufferArray, 0, result.Count)
                    do! fun ct -> ffmpeg.StandardInput.BaseStream.WriteAsync (data, ct)
                        |> Async.AwaitTaskWithCancellation
                bufferPool.Return (bufferArray, true)
                ffmpeg.StandardInput.BaseStream.Close ()
                do! ffmpeg.WaitForExitAsync () |> Async.AwaitTask
                // printfn "Finished encoding."
            }
        | _ -> 
            async {
                ctx.Response.StatusCode <- 400
                do! ctx.Response.WriteAsync ("Query parameters 'frameCount', 'name' and 'ffInput' are required exactly once, 'ffOutput' at least once.") |> Async.AwaitTask
            }

    let websocket ui ffmpegPath ffargs outputDir = WebSocket.handle (handleSocket ui ffmpegPath ffargs outputDir)

module Program =
    type Arguments =
        | [<AltCommandLine("-p")>] Port of port : int
        | [<AltCommandLine("-o"); Unique>] Output of path : string
        | [<AltCommandLine("-f"); Unique>] FFmpeg of path : string
        | FFargs of args : string
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Port _ -> "specify the port the server listens on (default: 5000)."
                | Output _ -> "specify the output folder (defaults to current working directory)."
                | FFmpeg _ -> "specify the path to the FFmpeg executable."
                | FFargs _ -> "extra arguments to pass to FFmpeg at the start of the argument list."

    let fuse (middlware : HttpContext -> (unit -> Async<unit>) -> Async<unit>) (app : IApplicationBuilder) =
        app.Use(fun env next ->
            middlware env (next.Invoke >> Async.AwaitTask)
            |> Async.StartAsTask :> Task )

    [<EntryPoint>]
    let main args =
        let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
        let argParser = ArgumentParser.Create<Arguments> (programName = "ffmpegserver", errorHandler = errorHandler)
        let args = argParser.Parse args

        let parsePort port =
            if port < 0 || port > int UInt16.MaxValue
            then failwithf "invalid port number: %i." port
            else port

        let parseOutput path =
            if Directory.Exists path
            then Path.GetFullPath path
            else failwithf "output directory does not exist."

        let parseFFmpeg path =
            if File.Exists path
            then Path.GetFullPath path
            else failwithf "could not find ffmpeg executable."

        let ports = 
            args.PostProcessResults (Port, parsePort) 
            |> List.defaultValue 5000
        let outputDirectory = 
            args.TryPostProcessResult (Output, parseOutput) 
            |> Option.defaultValue (Directory.GetCurrentDirectory())
        let ffmpegPath =
            args.TryPostProcessResult (FFmpeg, parseFFmpeg)
            |> Option.defaultValue "ffmpeg"
        let ffargs =
            args.TryGetResult (FFargs)
            |> Option.map (fun args -> args + " ")
            |> Option.defaultValue ""

        let config = 
            ConfigurationBuilder()
                .AddInMemoryCollection()
                .Build()

        let urls = ports |> List.map (fun p -> $"http://localhost:%i{p}") |> List.toArray

        let ui = ProgressUI.createUI (urls |> String.concat ", ")

        use host =
            Host.CreateDefaultBuilder()
                .ConfigureLogging(fun builder ->
                    builder
                        .ClearProviders ()
                        |> ignore )
                .ConfigureWebHostDefaults(fun builder ->
                    builder
                        .UseUrls(urls)
                        .UseConfiguration(config)
                        .Configure(fun app ->
                            app.UseWebSockets()
                            |> fuse (FFmpeg.websocket ui ffmpegPath ffargs outputDirectory)
                            |> ignore )
                        |> ignore )
                .Build()
        
        printfn "Starting FFmpegServer."

        host.Start ()

        Console.Clear ()
        printfn "Listening on %s" (urls |> String.concat ", ")
        printfn "Waiting for requests. Press Ctrl+C to shut down."

        host.WaitForShutdown ()
        0
