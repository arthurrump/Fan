module Animation.App.Render

open Animation.Animation
open Animation.Render

open Elmish
open Fable.React
open Fable.React.Props
open Browser.Types

module Selecting =
    type State<'t, 'r when 't : comparison> =
        { Scenes : Scene<'t, 'r> list
          Selected : Scene<'t, 'r> list
          ServerAddress : string }

    type Message<'t, 'r when 't : comparison> =
        | ToggleSelection of Scene<'t, 'r>
        | SelectAll
        | SelectNone
        | SetServerAddress of string
        | Done

    type ExternalMessage<'t, 'r when 't : comparison> =
        | DoneSelecting of server : string * selected : Scene<'t, 'r> list

    let init scenes =
        { Scenes = scenes
          Selected = []
          ServerAddress = "localhost:5000" }
        , Cmd.none
        , []

    let update msg model =
        match msg with
        | ToggleSelection scene ->  
            if model.Selected |> List.contains scene
            then { model with Selected = model.Selected |> List.except [scene] }
            else { model with Selected = scene :: model.Selected }
            , Cmd.none
            , Cmd.none
        | SelectAll ->
            { model with Selected = model.Scenes }
            , Cmd.none
            , Cmd.none
        | SelectNone ->
            { model with Selected = [] }
            , Cmd.none
            , Cmd.none
        | SetServerAddress server ->
            { model with ServerAddress = server }
            , Cmd.none
            , Cmd.none
        | Done ->
            model
            , Cmd.none
            , Cmd.ofMsg (DoneSelecting (model.ServerAddress, model.Selected))

    let view model dispatch =
        div [] [
            span [] [
                str "FFmpegServer address: "
                input [ 
                    Value (model.ServerAddress)
                    OnChange (fun ev -> dispatch (SetServerAddress (ev.target :?> HTMLInputElement).value))
                ]
            ]
            ul [] [
                for s in model.Scenes ->
                    li [] [
                        input [ 
                            Type "checkbox"
                            Checked (model.Selected |> List.contains s)
                            OnChange (fun _ -> dispatch (ToggleSelection s)) ]
                        str s.Title
                    ]
            ]
            span [] [ 
                button [ OnClick (fun _ -> dispatch SelectAll) ] [ str "Select all" ]
                button [ OnClick (fun _ -> dispatch SelectNone) ] [ str "Select none" ]
            ]
            button [ OnClick (fun _ -> dispatch Done) ] [ str "Done" ]
        ]

module Rendering =
    type RenderingSettings =
        { Server : string
          Settings : RenderSettings }

    type State<'t, 'r when 't : comparison> = 
        { Settings : RenderingSettings
          Queue : Scene<'t, 'r> list
          Current : Scene<'t, 'r> option
          Progress : int * int
          Finished : Scene<'t, 'r> list }

    type Message =
        | Start
        | ProgressUpdate of frame : int * total : int

    let init (settings, queue) =
        { Settings = settings
          Queue = queue
          Current = None
          Progress = 0, 0
          Finished = [] }
        , Cmd.ofMsg Start

    let private renderSceneCmd settings scene =
        Cmd.ofSub (fun dispatch ->
            let progress (frame, total) = dispatch (ProgressUpdate (frame, total))
            CanvasRender.runFFmpegCanvasRender settings.Server settings.Settings progress scene
        )

    let update msg model =
        match msg with
        | ProgressUpdate (frame, total) when frame = total ->
            { model with 
                Current = None
                Progress = 0, 0 
                Finished = 
                    match model.Current with
                    | Some cur -> cur :: model.Finished
                    | None -> model.Finished }
            , Cmd.ofMsg Start
        | ProgressUpdate (frame, total) ->
            { model with Progress = frame, total }
            , Cmd.none
        | Start when model.Current = None && not (model.Queue |> List.isEmpty) ->
            let scene = model.Queue |> List.head
            { model with
                Current = Some scene
                Queue = model.Queue |> List.tail
                Progress = 0, 0 }
            , renderSceneCmd model.Settings scene
        | Start ->
            model
            , Cmd.none

    let view model dispatch =
        div [] [
            match model.Current with
            | None -> ()
            | Some scene ->
                h1 [] [ str "Rendering "; str scene.Title ]
                span [] [ 
                    str (sprintf "%i/%i" (fst model.Progress) (snd model.Progress))
                    progress [ Value (fst model.Progress); Max (snd model.Progress) ] []
                ]
            if not (model.Queue |> List.isEmpty) then 
                h1 [] [ str "Queue" ]
                ul [] [ 
                    for s in model.Queue ->
                        li [] [ str s.Title ]
                ]
            if not (model.Finished |> List.isEmpty) then
                h1 [] [ str "Finished" ]
                ul [] [
                    for s in model.Finished ->
                        li [] [ str s.Title ]
                ]
        ]

type PageState<'t, 'r when 't : comparison> =
    | Selecting of Selecting.State<'t, 'r>
    | Rendering of Rendering.State<'t, 'r>

type State<'t, 'r when 't : comparison> =
    { RenderSettings : RenderSettings
      Page : PageState<'t, 'r> }

type Message<'t, 'r when 't : comparison> =
    | SelectingMsg of Selecting.Message<'t, 'r>
    | SelectingExtMsg of Selecting.ExternalMessage<'t, 'r>
    | RenderingMsg of Rendering.Message

let init (renderSettings, scenes) =
    let scenes = scenes |> List.mapi (fun i -> Scene.mapTitle (fun t -> $"%02i{i}_%s{t}"))
    let state, cmd, extCmd = Selecting.init scenes
    { RenderSettings = renderSettings
      Page = Selecting state }
    , Cmd.batch [ cmd |> Cmd.map SelectingMsg; extCmd |> Cmd.map SelectingExtMsg ]

let update msg model =
    let page, cmd =
        match model.Page, msg with
        | Selecting state, SelectingMsg msg ->
            let state, cmd, extCmd = Selecting.update msg state
            Selecting state
            , Cmd.batch [ cmd |> Cmd.map SelectingMsg; extCmd |> Cmd.map SelectingExtMsg ]
        | Selecting _, SelectingExtMsg (Selecting.DoneSelecting (server, scenes)) ->
            let settings : Rendering.RenderingSettings = { Server = server; Settings = model.RenderSettings }
            let state, cmd = Rendering.init (settings, scenes)
            Rendering state
            , cmd |> Cmd.map RenderingMsg
        | Rendering state, RenderingMsg msg ->
            let state, cmd = Rendering.update msg state
            Rendering state
            , cmd |> Cmd.map RenderingMsg
        | Selecting _, RenderingMsg _ 
        | Rendering _, SelectingMsg _ 
        | Rendering _, SelectingExtMsg _ ->
            model.Page
            , Cmd.none
    { model with Page = page }
    , cmd

let view model dispatch =
    match model.Page with
    | Selecting model ->
        Selecting.view model (SelectingMsg >> dispatch)
    | Rendering model ->
        Rendering.view model (RenderingMsg >> dispatch)
