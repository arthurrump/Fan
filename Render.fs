module Render

open FSharp.Data.UnitSystems.SI.UnitSymbols
[<Measure>] type ms

open Animation
open Browser
open Browser.Dom
open Browser.Types
open Browser.Url
open Fable.Core
open Fable.Core.JsInterop
open FFmpeg

type Blob' =
    inherit Blob
    abstract member arrayBuffer : unit -> JS.Promise<JS.ArrayBuffer>

type [<AllowNullLiteral>] OffscreenCanvas =
    abstract height : float with get, set
    abstract width : float with get, set
    [<Emit("$0.getContext('2d'{{, $1}})")>] 
    abstract getContext_2d: ?contextAttributes: obj -> CanvasRenderingContext2D
    abstract getContext : contextId : string * ?contextAttributes : obj -> obj

type [<AllowNullLiteral>] OffscreenCanvasType =
    [<Emit("new $0($1...)")>]
    abstract Create : width : int * height : int -> OffscreenCanvas

let [<Global>] OffscreenCanvas : OffscreenCanvasType = jsNative

let receiveMessage<'t> (worker : Worker) =
    Async.FromContinuations (fun (success, error, cancel) ->
        worker.addEventListener (
            "message", 
            (fun ev -> success (unbox<'t> (ev :?> MessageEvent).data)), 
            jsOptions<AddEventListenerOptions> (fun x -> x.once <- true)
        )
    )

type RenderSettings =
    { Width : int
      Height : int
      Framerate : float</s> }

let defaultSettings =
    { Width = 300
      Height = 150
      Framerate = 30.</s> }

let private ffmpegRawToProres4444 settings inFile outFile  =
    [| // Force the input format to raw video
       "-f"; "rawvideo" 
       // The raw data is in RGBA format, 4 bytes per pixel
       "-pixel_format"; "rgba" 
       // Get settings for the video parameters
       "-video_size"; $"%i{settings.Width}x%i{settings.Height}"
       "-framerate"; string settings.Framerate
       // Read the data from this file
       "-i"; inFile 
       // Use the prores_ks encoder
       "-c:v"; "prores_ks" 
       // With the ProRes 4444 profile
       "-profile:v"; "4" 
       // Use YUVA pixel format with alpha channel
       "-pix_fmt"; "yuva444p10le" 
       // Set some flags that make this look like it's generated by an Apple product
       "-movflags"; "write_colr" 
       "-flags"; "bitexact" 
       "-chunk_duration"; "500K"
       "-metadata:s"; "\"encoder=Apple ProRes 4444\"" 
       "-vendor"; "apl0" 
       "-timecode"; "10:00:00:00" 
       // Write the output to this file (should have .mov extension)
       outFile
    |]

let runRender scene =
    let settings = { defaultSettings with Width = 1920; Height = 1080; Framerate = 25.</s> }
    let canvas = OffscreenCanvas.Create (settings.Width, settings.Height)
    let ctx = canvas.getContext_2d ()
    let render = 
        let r = Scene.getRenderFunction false ctx scene
        fun (t : float<ms>) ->
            r (float t)
            let image = ctx.getImageData (0., 0., canvas.width, canvas.height)
            image.data
    let dt : float<ms> = 1000.<ms/s> / settings.Framerate
    let dur = LanguagePrimitives.FloatWithMeasure (Scene.singleDuration scene)
    let resPromise =
        async {
            printfn "Loading FFmpeg"
            let ffmpeg = FFmpeg.createFFmpeg(jsOptions<CreateFFmpegOptions> (fun x -> x.log <- true))
            do! ffmpeg.load () |> Async.AwaitPromise
            // let dur = min dur 5000.<ms>
            printfn "Rendering canvas to FFmpeg filesystem"
            let inputStream = ffmpeg.FS ("open", "video.raw", "ax")
            let stat = ffmpeg.FS ("stat", "video.raw")
            console.log ("File:", stat, "isSocket:", ffmpeg.FS ("isSocket", (stat?mode :> obj)), "isFile:", ffmpeg.FS ("isFile", (stat?mode :> obj)))
            for t in 0.<ms> .. dt .. (dur + dt) do
                let buffer = render t
                ffmpeg.FS ("write", inputStream, buffer, 0, buffer.Length) |> ignore
            printfn "Finished rendering raw data, start encoding"
            do! ffmpeg.run (ffmpegRawToProres4444 settings "video.raw" "video.mov") |> Async.AwaitPromise
            printfn "Finished encoding, fetching file and cleaning up" 
            ffmpeg.FS ("close", inputStream) |> ignore
            ffmpeg.FS ("unlink", "video.raw") |> ignore
            let result = ffmpeg.FS ("readFile", "video.mov") :?> JS.Uint8Array
            ffmpeg.FS ("unlink", "video.mov") |> ignore
            return result
        } |> Async.StartAsPromise
    resPromise.``then``(fun video ->
        let file = File.Create ([| video |], "video.mov", jsOptions<FilePropertyBag> (fun x -> x.``type`` <- "video/quicktime"))
        let url = URL.createObjectURL file
        let a = document.createElement "a" :?> HTMLAnchorElement
        a.href <- url
        a.innerText <- sprintf "Download %s" "video.mov"
        a.setAttribute ("download", "video.mov")
        document.body.appendChild a |> ignore
    )
