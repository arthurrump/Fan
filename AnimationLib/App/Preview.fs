module Animation.App.Preview

open Animation.Animation

open Browser.Dom
open Browser.Types

open Elmish
open Fable.React
open Fable.React.Props
open Thoth.Json

type SceneStage = Enter | Run | Leave
type Playing = Playing of lastFrameTime : float | Paused

type PreviewState<'t, 'r when 't : comparison> =
    { CurrentScene : Scene<'t, 'r>
      SceneStage : SceneStage
      Time : float
      Playing : Playing }

    member state.Encode () =
        Encode.object [
            "currentSceneTitle", Encode.string state.CurrentScene.Title
            "sceneStage", Encode.Auto.generateEncoder<SceneStage>() state.SceneStage
            "time", Encode.float state.Time
        ]

    static member Decoder (scenes : Scene<'t, 'r> list) =
        Decode.object <| fun get ->
            let sceneTitle = get.Required.Field "currentSceneTitle" Decode.string
            { CurrentScene = 
                scenes 
                |> List.tryFind (fun s -> s.Title = sceneTitle) 
                |> Option.defaultValue (scenes |> List.head)
              SceneStage = get.Required.Field "sceneStage" (Decode.Auto.generateDecoder<SceneStage>())
              Time = get.Required.Field "time" Decode.float
              Playing = Paused }
   
type PreviewMessage<'t, 'r when 't : comparison> =
    | SetScene of Scene<'t, 'r>
    | SetStage of SceneStage
    | SetTime of float
    | AnimationFrameGranted of time : float
    | StartPlaying
    | PausePlaying

let private renderScene ctx (model : PreviewState<'t, 'r>) =
    let render =
        match model.SceneStage with
        | Enter -> Scene.getEnterRenderFunction
        | Run -> Scene.getRunRenderFunction
        | Leave -> Scene.getLeaveRenderFunction
    render ctx model.CurrentScene model.Time

let private maxTime model =
    match model.SceneStage with
    | Enter -> model.CurrentScene.EnterAnimation.Duration |> Animation.singleDuration
    | Run -> model.CurrentScene.RunAnimation.Duration |> Animation.singleDuration
    | Leave -> model.CurrentScene.LeaveAnimation.Duration |> Animation.singleDuration

let private requestAnimationFrameCmd<'t, 'r when 't : comparison> : Cmd<PreviewMessage<'t, 'r>> = 
    Cmd.ofSub (fun dispatch ->
        window.requestAnimationFrame (fun t ->
            dispatch (AnimationFrameGranted t)
        ) |> ignore
    )

let defaultState scenes = 
    { CurrentScene = scenes |> List.head
      SceneStage = Enter
      Time = 0.
      Playing = Paused }, Cmd.none

let update msg model =
    match msg with
    | SetScene scene -> 
        { model with CurrentScene = scene; Time = 0. }, Cmd.none
    | SetStage stage -> 
        { model with SceneStage = stage; Time = 0. }, Cmd.none
    | SetTime time -> 
        { model with Time = time; Playing = Paused }, Cmd.none
    | AnimationFrameGranted time ->
        match model.Playing with
        | Playing lastFrameTime -> 
            let dt = time - lastFrameTime
            if model.Time + dt < maxTime model then
                { model with Playing = Playing time; Time = model.Time + dt }, requestAnimationFrameCmd
            else
                { model with Playing = Paused; Time = maxTime model }, Cmd.none
        | Paused ->
            model, Cmd.none
    | StartPlaying ->
        { model with 
            Playing = Playing (Fable.Core.JsInterop.emitJsExpr () "performance.now()") 
            Time = if model.Time = maxTime model then 0. else model.Time }
        , requestAnimationFrameCmd
    | PausePlaying ->
        { model with Playing = Paused }, Cmd.none

let view ctx (scenes : Scene<'t, 'r> list) (model : PreviewState<'t, 'r>) dispatch =
    let dispatchButton message value style =
        input [ 
            Type "button"
            OnClick (fun _ -> dispatch message)
            Value value
            Style style
        ]
    do renderScene ctx model
    div [ Style [ Padding "1em" ] ] [
        input [ 
            Type "range"
            Min 0.; Max (maxTime model)
            Value model.Time
            OnChange (fun ev -> dispatch (SetTime (ev.target :?> HTMLInputElement).valueAsNumber))
            Style [ Width "100%" ] ]
        match model.Playing with
        | Playing _ -> dispatchButton PausePlaying "Pause" []
        | Paused -> dispatchButton StartPlaying "Play" []
        p [] [
            dispatchButton (SetStage Enter) "Enter" [ if model.SceneStage = Enter then FontWeight "bold" ]
            dispatchButton (SetStage Run) "Run" [ if model.SceneStage = Run then FontWeight "bold" ]
            dispatchButton (SetStage Leave) "Leave" [ if model.SceneStage = Leave then FontWeight "bold" ]
        ]
        p [] [
            for i, scene in scenes |> List.indexed do
                dispatchButton (SetScene scene) $"{i}: {scene.Title}" [ if model.CurrentScene.Title = scene.Title then FontWeight "bold" ]
        ]
    ]
