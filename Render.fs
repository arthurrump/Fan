module Render

open Animation
open Browser.Dom
open Browser.Types
open Browser.Url
open Fable.Core
open Fable.Core.JsInterop

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
      FramerateNumerator : int
      FramerateDenominator : int
      Bitrate : int }

let defaultSettings =
    { Width = 300
      Height = 150
      FramerateNumerator = 1
      FramerateDenominator = 30
      Bitrate = 200 }

let webmRender settings render ts =
    async {
        printfn "Starting webm worker"
        // 1. Load the worker
        let webmWorker = Worker.Create "node_modules/webm-wasm/dist/webm-worker.js"
        try
            // 2. Send the path to the webm wasm file
            webmWorker.postMessage "./webm-wasm.wasm"
            // 3. Wait for the worker to be ready
            let! mes = receiveMessage<string> webmWorker
            printfn "Got message: %s" mes
            // 4. Send the configuration parameters
            webmWorker.postMessage
                {| width = settings.Width
                   height = settings.Height
                   timebaseNum = settings.FramerateNumerator
                   timebaseDen = settings.FramerateDenominator
                   bitrate = settings.Bitrate |}
            // 5. Send frames
            for t in ts do
                if ceil (10. * t) % 10. = 0. then printfn "Rendering t=%f" t
                let imageBuffer : JS.ArrayBuffer = render t
                webmWorker.postMessage (imageBuffer, [| imageBuffer |])
            // 6. Signal end-of-stream
            webmWorker.postMessage null
            printfn "Done rendering."
            // 7. Get the webm file as an array of bytes
            let! webm = receiveMessage<JS.ArrayBuffer> webmWorker
            printfn "Received result"
            return webm
        finally
            // 8. Cleanup
            webmWorker.terminate ()
    }

let runRender scene =
    async {
        printfn "runRender"
        let settings = { defaultSettings with Width = 1920; Height = 1080; FramerateDenominator = 25 }
        let canvas = OffscreenCanvas.Create (settings.Width, settings.Height)
        let ctx = canvas.getContext_2d ()
        let render = 
            let r = Scene.getRunRenderFunction ctx scene
            fun t ->
                r t
                let image = ctx.getImageData (0., 0., canvas.width, canvas.height)
                image.data.buffer
        let dt = 1000. * float settings.FramerateNumerator / float settings.FramerateDenominator
        let dur = singleDuration scene.RunAnimation.Duration
        let filename = "video.webm"
        printfn "Duration: %f; No of frames: %i" dur ([0. .. dt .. dur].Length)
        let! webm = webmRender settings render [0. .. dt .. dur]
        let file = File.Create ([| webm |], filename, jsOptions<FilePropertyBag> (fun x -> x.``type`` <- "video/webm"))
        let url = URL.createObjectURL file
        let a = document.createElement "a" :?> HTMLAnchorElement
        a.href <- url
        a.innerText <- sprintf "Download %s" filename
        a.setAttribute ("download", filename)
        document.body.appendChild a |> ignore
        return ()
    } |> Async.Start