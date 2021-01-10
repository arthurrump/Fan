module Animation.App.Main

open System
open Browser.Dom
open Browser.Types
open Fable.Core.JsInterop

open Elmish
open Elmish.React
open Thoth.Json

let throttle t (f : 'a -> unit) =
    let mutable timeout = None
    let mutable lastArg = None
    let rec exec x =
        lastArg <- None
        f x
        timeout <- Some (window.setTimeout (onTimeout, t))
    and onTimeout _ =
        match lastArg with
        | Some x -> exec x
        | None -> timeout <- None
    fun (x : 'a) ->
        match timeout with
        | Some _ -> lastArg <- Some x
        | None -> exec x

let withHashState program =
    let updateHistory = throttle 500 (fun hash -> history.replaceState ((), url = "#" + window.btoa hash))
    let urlUpdate update msg model =
        let (newModel : Preview.PreviewState<'t, 'r>, cmd) = update msg model
        let hash = Encode.toString 0 (newModel.Encode ())
        updateHistory hash
        newModel, cmd
    Program.map id urlUpdate id id id program

let runApp preview containerId scenes =
    let init () = 
        let hash = window.location.hash.TrimStart '#' |> window.atob
        if String.IsNullOrWhiteSpace hash then 
            Preview.defaultState scenes
        else 
            match Decode.fromString (Preview.PreviewState.Decoder scenes) hash with
            | Ok state -> 
                state, Cmd.none
            | Error msg ->
                console.error ("Error parsing state hash:", msg)
                Preview.defaultState scenes

    let style = document.createElement "style"
    style.innerHTML <- """
        :root.scrollbar-hidden {
            scrollbar-width: none;
        }
        :root.scrollbar-hidden::-webkit-scrollbar {
            display: none;
        }
    """
    document.getElementsByTagName("head").[0].appendChild style |> ignore

    let setScrollbarVisibility _ =
        let html = document.getElementsByTagName("html").[0]
        if window?fullScreen || window.innerWidth = window.screen.width && window.innerHeight = window.screen.height
        then html.classList.add "scrollbar-hidden"
        else html.classList.remove "scrollbar-hidden"

    window.onresize <- setScrollbarVisibility
    setScrollbarVisibility ()

    let appContainerId, drawingCtx, setPreviewVisible = preview (document.getElementById containerId)

    Program.mkProgram (init) (Preview.update) (Preview.view drawingCtx scenes)
    |> withHashState
    |> Program.withReactBatched appContainerId
    |> Program.run

type CanvasAppSettings =
    { Width : int
      Height : int
      BackgroundColor : string }

let runCanvasApp settings =
    let preview (container : HTMLElement) =
        let id x = 
            if container.id <> null 
            then container.id + "_animation_" + x
            else "animation_" + x

        let canvas = document.createElement "canvas" :?> HTMLCanvasElement
        canvas.id <- id "canvas"
        canvas.width <- float settings.Width
        canvas.height <- float settings.Height
        canvas?style?``max-width`` <- "100%"
        canvas?style?margin <- 0
        canvas?style?padding <- 0
        canvas?style?``background-color`` <- settings.BackgroundColor
        let ctx = canvas.getContext_2d ()
        let setCanvasVisible visible =
            if visible
            then canvas?style?display <- "unset"
            else canvas?style?display <- "hidden"

        let div = document.createElement "div"
        div?style?margin <- 0
        div?style?padding <- "1em"
        div.id <- id "app"
        
        container?style?margin <- 0
        container?style?padding <- 0
        container.appendChild canvas |> ignore
        container.appendChild div |> ignore

        div.id, ctx, setCanvasVisible

    runApp preview
