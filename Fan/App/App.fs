module Fan.App.Main

open Fan.Animation
open Fan.Render

open System
open Browser.Dom
open Browser.Types
open Fable.Core.JsInterop

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Thoth.Json

let private throttle t (f : 'a -> unit) =
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

let private withHashState encode program =
    let updateHistory = throttle 500 (fun hash -> history.replaceState ((), url = "#" + window.btoa hash))
    let urlUpdate update msg model =
        let (newModel, cmd) = update msg model
        let hash = Encode.toString 0 (encode newModel)
        updateHistory hash
        newModel, cmd
    Program.map id urlUpdate id id id program

type private Message<'t, 'r when 't : comparison> =
    | PreviewMsg of Preview.Message<'t, 'r>
    | RenderMsg of Render.Message<'t, 'r>
    | GotoPreview
    | GotoRender

type private Page<'t, 'r when 't : comparison> =
    | Preview of Preview.State<'t, 'r>
    | Render of Render.State<'t, 'r>

    member this.Encode () =
        match this with
        | Preview state -> Encode.object [ "page", Encode.string "Preview"; "state", state.Encode () ]
        | Render _ -> Encode.object [ "page", Encode.string "Render" ]

    static member Decoder (settings, scenes) =
        Decode.field "page" Decode.string
        |> Decode.andThen (function
            | "Preview" -> 
                Decode.field "state" (Preview.initDecode scenes)
                |> Decode.map (fun (state, cmd) -> Preview state, Cmd.map PreviewMsg cmd)
            | "Render" -> 
                Render.initDecode (settings, scenes)
                |> Decode.map (fun (state, cmd) -> Render state, Cmd.map RenderMsg cmd)
            | _ -> 
                Decode.fail "Invalid value for field 'page'")

type private State<'t, 'r when 't : comparison> =
    { RenderSettings : RenderSettings
      Scenes : Scene<'t, 'r> list
      Page : Page<'t, 'r> }

    member this.Encode () =
        this.Page.Encode ()

    static member Decoder (renderSettings, scenes) =
        Page<'t, 'r>.Decoder (renderSettings, scenes)
        |> Decode.map (fun (page, cmd) ->
            { RenderSettings = renderSettings
              Scenes = scenes
              Page = page }
            , cmd
        )

let private init (renderSettings, scenes : Scene<'t, 'r> list) : State<'t, 'r> * Cmd<Message<'t, 'r>> =
    let state, cmd = Preview.init scenes
    { RenderSettings = renderSettings
      Scenes = scenes
      Page = Preview state }
    , cmd |> Cmd.map PreviewMsg

let private initFromHash<'t, 'r when 't : comparison> (renderSettings, scenes : Scene<'t, 'r> list) =
    let hash = window.location.hash.TrimStart '#' |> window.atob
    if String.IsNullOrWhiteSpace hash then 
        init (renderSettings, scenes)
    else 
        match Decode.fromString (State<'t, 'r>.Decoder (renderSettings, scenes)) hash with
        | Ok (state, cmd) -> 
            state, cmd
        | Error msg ->
            console.error ("Error parsing state hash:", msg)
            init (renderSettings, scenes)

let private update msg model =
    match model.Page, msg with
    | Preview state, PreviewMsg msg ->
        let state, cmd = Preview.update msg state
        { model with Page = Preview state }
        , cmd |> Cmd.map PreviewMsg
    | Preview _, GotoRender ->
        let state, cmd = Render.init (model.RenderSettings, model.Scenes)
        { model with Page = Render state }
        , cmd |> Cmd.map RenderMsg
    | Render state, RenderMsg msg ->
        let state, cmd = Render.update msg state
        { model with Page = Render state }
        , cmd |> Cmd.map RenderMsg
    | Render _, GotoPreview ->
        let state, cmd = Preview.init (model.Scenes)
        { model with Page = Preview state }
        , cmd |> Cmd.map PreviewMsg
    | _ ->
        model
        , Cmd.none

let private view drawingCtx setPreviewVisible model dispatch =
    div [] [
        nav [] [
            button [ OnClick (fun _ -> dispatch GotoPreview) ] [ str "Preview" ]
            button [ OnClick (fun _ -> dispatch GotoRender) ] [ str "Render" ]
        ]
        match model.Page with
        | Preview model ->
            do setPreviewVisible true
            Preview.view drawingCtx model (PreviewMsg >> dispatch)
        | Render model ->
            do setPreviewVisible false
            Render.view model (RenderMsg >> dispatch)
    ]

let runApp preview containerId renderSettings scenes =
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

    Program.mkProgram initFromHash update (view drawingCtx setPreviewVisible)
    |> withHashState (fun x -> x.Encode ())
    |> Program.withReactBatched appContainerId
    |> Program.runWith (renderSettings, scenes)

type CanvasAppSettings =
    { RenderSettings : RenderSettings
      BackgroundColor : string }

let runCanvasApp settings containerId =
    let preview (container : HTMLElement) =
        let id x = 
            if container.id <> null 
            then container.id + "_animation_" + x
            else "animation_" + x

        let canvas = document.createElement "canvas" :?> HTMLCanvasElement
        canvas.id <- id "canvas"
        canvas.width <- float settings.RenderSettings.Width
        canvas.height <- float settings.RenderSettings.Height
        canvas?style?``max-width`` <- "100%"
        canvas?style?margin <- 0
        canvas?style?padding <- 0
        canvas?style?``background-color`` <- settings.BackgroundColor
        let ctx = canvas.getContext_2d ()
        let setCanvasVisible visible =
            if visible
            then canvas?style?display <- "unset"
            else canvas?style?display <- "none"

        let div = document.createElement "div"
        div?style?margin <- 0
        div?style?padding <- "1em"
        div.id <- id "app"
        
        container?style?margin <- 0
        container?style?padding <- 0
        container.appendChild canvas |> ignore
        container.appendChild div |> ignore

        div.id, ctx, setCanvasVisible

    runApp preview containerId settings.RenderSettings
