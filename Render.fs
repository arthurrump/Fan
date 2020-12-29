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
      FramerateDenominator : int }

let defaultSettings =
    { Width = 300
      Height = 150
      FramerateNumerator = 1
      FramerateDenominator = 30 }

let runRender scene =
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
    let filename = "video.raw"
    let frames = [| 0. .. dt .. (dur + dt) |] |> Array.map (render >> box)
    let file = File.Create (frames, filename, jsOptions<FilePropertyBag> (fun x -> x.``type`` <- "video/raw"))
    let url = URL.createObjectURL file
    let a = document.createElement "a" :?> HTMLAnchorElement
    a.href <- url
    a.innerText <- sprintf "Download %s" filename
    a.setAttribute ("download", filename)
    document.body.appendChild a |> ignore