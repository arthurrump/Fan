module Animation

open System
open Browser.Dom
open Thoth.Json

module List =
    let duplicates list =
        let (duplicates, _) =
            List.mapFold (fun found elem -> 
                if found |> Set.contains elem 
                then Some elem, found 
                else None, found |> Set.add elem
            ) (Set.empty) list
        duplicates |> List.choose id
    let reduceOrDefault reducer def list =
        if list |> List.isEmpty
        then def
        else list |> List.reduce reducer
    let maxOrDefault def list =
        if list |> List.isEmpty
        then def
        else list |> List.max

module Map =
    let combine map1 map2 =
        map1 |> Map.fold (fun map2 key value -> map2 |> Map.add key value) map2

type Easing = 
    | Linear
    | EaseInSine | EaseOutSine | EaseInOutSine
    | EaseInQuad | EaseOutQuad | EaseInOutQuad
    | EaseInCubic | EaseOutCubic | EaseInOutCubic
    | EaseInQuart | EaseOutQuart | EaseInOutQuart
    | EaseInQuint | EaseOutQuint | EaseInOutQuint
    | EaseInExpo | EaseOutExpo | EaseInOutExpo
    | EaseInCirc | EaseOutCirc | EaseInOutCirc
    | EaseInBack | EaseOutBack | EaseInOutBack
    | EaseInElastic | EaseOutElastic | EaseInOutElastic
    | Custom of (float -> float)

// https://easings.net/
let private easingFunction : Easing -> (float -> float) = 
    let inpow p = fun x -> x**p
    let outpow p = fun x -> 1. - (1. - x)**p
    let inoutpow p = fun x -> if x < 0.5 then 2.**(p-1.) * x**p else 1. - (-2.*x + 2.)**p / 2.
    let c1 = 1.70158
    let c2 = c1 * 1.525
    let c3 = c1 + 1.
    let c4 = (2. * Math.PI) / 3.
    let c5 = (2. * Math.PI) / 4.5
    function
    | Linear -> fun x -> x
    | EaseInSine -> fun x -> 1. - cos(x * Math.PI / 2.)
    | EaseOutSine -> fun x -> sin(x * Math.PI / 2.)
    | EaseInOutSine -> fun x -> -(cos(Math.PI * x) - 1.) / 2.
    | EaseInQuad -> inpow 2.
    | EaseOutQuad -> outpow 2.
    | EaseInOutQuad -> inoutpow 2.
    | EaseInCubic -> inpow 3.
    | EaseOutCubic -> outpow 3.
    | EaseInOutCubic -> inoutpow 3.
    | EaseInQuart -> inpow 4.
    | EaseOutQuart -> outpow 4.
    | EaseInOutQuart -> inoutpow 4.
    | EaseInQuint -> inpow 5.
    | EaseOutQuint -> outpow 5.
    | EaseInOutQuint -> inoutpow 5.
    | EaseInExpo -> 
        fun x -> if x = 0. then 0. else 2.**(10.*x - 10.)
    | EaseOutExpo -> 
        fun x -> if x = 1. then 1. else 1. - 2.**(-10.*x)
    | EaseInOutExpo -> 
        fun x -> if x = 0. || x = 1. then x 
                 elif x < 0.5 then 2.**(20.*x - 10.) / 2. 
                 else (2. - 2.**(-20.*x + 10.)) / 2.
    | EaseInCirc ->
        fun x -> 1. - sqrt(1. - x**2.)
    | EaseOutCirc ->
        fun x -> sqrt(1. - (x - 1.)**2.)
    | EaseInOutCirc ->
        fun x -> if x < 0.5 then (1. - sqrt(1. - (2.*x)**2.)) / 2. 
                 else (sqrt(1. - (-2.*x + 2.)**2.) + 1.) / 2.
    | EaseInBack ->
        fun x -> c3 * x**3. - c1 * x**2.
    | EaseOutBack ->
        fun x -> 1. + c3 * (x-1.)**3. + c1 * (x-1.)**2.
    | EaseInOutBack ->
        fun x -> if x < 0.5 then ((2.*x)**2. * ((c2+1.) * 2. * x - c2)) / 2. 
                 else ((2.*x - 2.)**2. * ((c2+1.) * (x*2. - 2.) + c2) + 2.) / 2.
    | EaseInElastic ->
        fun x -> if x = 0. || x = 1. then x else -2.**(10.*x - 10.) * sin((x*10. - 10.75) * c4)
    | EaseOutElastic ->
        fun x -> if x = 0. || x = 1. then x else 2.**(-10.*x) * sin((x*10. - 0.75) * c4) + 1.
    | EaseInOutElastic ->
        fun x -> if x = 0. || x = 1. then x
                 elif x < 0.5 then -(2.**(20.*x - 10.) * sin((20.*x - 11.125) * c5)) / 2.
                 else (2.**(-20.*x + 10.) * sin((20.*x - 11.125) * c5)) / 2. + 1.
    | Custom f -> f

type Key = 
    { Value : float
      Easing : Easing }

let (=>) x y = x, y

type Var<'t> = 't * Key
type VarsBuilder<'t>() =
    member __.Zero () = []
    member __.Yield ((var, (v, e))) = [ var, { Value = v; Easing = e } ]
    member this.Yield ((var, (v, e)) : 't * (int * Easing)) = this.Yield ((var, (float v, e)))
    member this.Yield ((var, v) : 't * float) = this.Yield ((var, (v, Linear)))
    member this.Yield ((var, v) : 't * int) = this.Yield ((var, float v))
    member __.Combine (x : Var<'t> list, y) = x @ y
    member __.Delay (f) = f()

let vars<'t> = VarsBuilder<'t>()

type Timestamp<'t> = float * Var<'t> list
type Direction = 
    | Normal 
    | Reverse 
    | Alternate
type Loop = 
    | Repeat of int 
    | Infinite
type Timeline<'t> =
    { Timestamps : Timestamp<'t> list
      Delay : float
      Direction : Direction
      Loop : Loop }

module Timeline =
    let inline delay delay tl =
        { tl with Delay = (float delay) }
    
    let duration tl =
        let repeat = 
            match tl.Loop with
            | Repeat x -> x
            | Infinite -> 1
        tl.Delay + (tl.Timestamps |> List.map fst |> List.maxOrDefault 0.) * float repeat

type TimelineBuilder(?direction, ?loop) =
    let direction = defaultArg direction Normal
    let loop = defaultArg loop (Repeat 1)

    member __.Zero () = 
        []
    member __.Yield (ts : Timestamp<'t>) = 
        [ ts ]
    member this.Yield ((t, var) : int * Var<'t> list) = 
        this.Yield ((float t, var))
    member __.Delay (f) = 
        f()
    member __.Combine (t1 : Timestamp<'t> list, t2) = 
        t1 @ t2
    member __.Run (timestamps) = 
        { Timestamps = timestamps; Delay = 0.; Direction = direction; Loop = loop }

let timeline = TimelineBuilder()
let timeline' (direction, loop) = TimelineBuilder (direction, loop)

let private calculateTimeline initials timeline =
    let ts = timeline.Timestamps |> List.sortBy fst
    let totalDur = ts |> List.tryLast |> Option.map fst |> Option.defaultValue 0.
    let chooseKeys var (t, vars) = 
        vars 
        |> List.tryFind (fun (v, _) -> v = var) 
        |> Option.map (fun (_, key) -> (t, key))
    let getKeys var = 
        let keys = ts |> List.choose (chooseKeys var)
        let initialValue = initials |> Map.tryFind var |> Option.defaultValue (snd keys.Head).Value
        keys 
        |> List.mapFold (fun (startTime, prev) (endTime, key) ->
            let valueFunc t = 
                let duration = endTime - startTime
                let progression = (t - startTime) / duration
                let easedProgression = easingFunction key.Easing progression
                prev + (key.Value - prev) * easedProgression
            {| StartTime = startTime; EndTime = endTime
               Value = key.Value; ValueFunc = valueFunc |},
            (endTime, key.Value)) (0., initialValue)
        |> fst
    let vars = ts |> List.collect snd |> List.map fst |> List.distinct
    let initialOnly = 
        let initialVars = initials |> Map.toSeq |> Seq.map fst |> Set.ofSeq
        initialVars - (vars |> Set.ofList)
        |> Set.toList
        |> List.map (fun v ->
            let value = initials.[v]
            let key = 
                {| StartTime = 0.; EndTime = totalDur
                   Value = value; ValueFunc = fun _ -> value |}
            v, [ key ])
    vars
    |> List.map (fun var -> var, getKeys var)
    |> List.append initialOnly
    |> Map.ofList
    |> Map.map (fun _ keys ->    
        fun t -> 
            let t = t - timeline.Delay
            let t =
                match timeline.Direction, timeline.Loop with
                | Normal, Infinite -> 
                    t % totalDur
                | Reverse, Infinite -> 
                    totalDur - (t % totalDur)
                | Alternate, Infinite -> 
                    let t = t % (2. * totalDur)
                    if t > totalDur then 2. * totalDur - t else t
                | Normal, Repeat i ->
                    if t < float i * totalDur
                    then t % totalDur
                    else totalDur
                | Reverse, Repeat i ->
                    if t < float i * totalDur
                    then totalDur - (t % totalDur)
                    else 0.
                | Alternate, Repeat i ->
                    if t < float i * totalDur then
                        let t = t % (2. * totalDur)
                        if t > totalDur then 2. * totalDur - t else t
                    else float (i % 2) * totalDur
            let key = keys |> List.tryFind (fun key -> key.StartTime <= t && t < key.EndTime)
            match key with
            | Some key ->   
                key.ValueFunc t
            | None when t < keys.Head.StartTime || keys.Length = 1 ->
                keys.Head.Value
            | None ->
                let key = keys |> List.findBack (fun key -> t >= key.EndTime)
                key.Value
    )

let rec private gcd x y = if y = 0. then abs x else gcd y (x % y)
let private lcm x y = x * y / (gcd x y)

type AnimationDuration = 
    | FixedDuration of float 
    | InfiniteDuration of initialBlock : float * loop : float

let isInfinite = function
    | InfiniteDuration _ -> true
    | _ -> false

let singleDuration = function
    | FixedDuration d -> d
    | InfiniteDuration (initial, _) -> initial

type Animation<'t when 't : comparison>(timelines : Timeline<'t> list, initials : Map<'t, float>) =
    let functionsMap = lazy (
        let animationFunctions = 
            timelines 
            |> List.collect (calculateTimeline initials >> Map.toList)
        do for var in animationFunctions |> List.map fst |> List.duplicates do
            console.warn (
                "Variable", var, "is defined in multiple parallel timelines.",
                "Only the definition in the first timeline will be used.")
        animationFunctions 
        |> List.distinctBy fst 
        |> Map.ofList
    )

    new(timelines : Timeline<'t> list) = Animation(timelines, Map.empty)
    member __.WithTimeline (tl) = Animation(tl::timelines, initials)
    member __.WithTimelines (tl) = Animation(tl @ timelines, initials)
    member __.WithInitial (key, value) = Animation(timelines, initials |> Map.add key value)
    member __.WithInitials (i) = Animation(timelines, Map.combine i initials)

    member __.Timelines = 
        timelines
    member __.Variables = 
        timelines 
        |> List.collect (fun tl -> tl.Timestamps)
        |> List.collect snd
        |> List.map fst
        |> List.append (initials |> Map.toList |> List.map fst)
        |> List.distinct
    member val Duration = 
        if timelines |> List.isEmpty then 
            FixedDuration 0.
        elif timelines |> List.exists (fun tl -> tl.Loop = Infinite) then 
            let loopDuration = 
                timelines 
                |> List.filter (fun tl -> tl.Loop = Infinite) 
                |> List.map Timeline.duration
                |> List.reduceOrDefault lcm 0.
            let maxFiniteDuration =
                timelines
                |> List.filter (fun tl -> tl.Loop <> Infinite)
                |> List.map Timeline.duration
                |> List.maxOrDefault 0.
            let initialIterations =
                Seq.initInfinite ((+) 1)
                |> Seq.find (fun i -> float i * loopDuration >= maxFiniteDuration)
            InfiniteDuration (float initialIterations * loopDuration, loopDuration)
        else 
            timelines |> List.map Timeline.duration |> List.max |> FixedDuration
    member __.Item (var) = 
        functionsMap.Value.[var]
    member __.TryItem (var) = 
        functionsMap.Value |> Map.tryFind var

type AnimationBuilder<'t when 't : comparison>() =
    member __.Zero () =
        [ timeline.Zero () |> timeline.Run ]
    member __.Yield ((delay, timeline) : float * Timeline<'t>) =
        [ { timeline with Delay = delay } ]
    member this.Yield ((delay, timeline) : int * Timeline<'t>) =
        this.Yield ((float delay, timeline))
    member __.Yield (timeline : Timeline<'t>) =
        [ timeline ]
    member __.YieldFrom (timelines : #seq<Timeline<'t>>) =
        timelines |> Seq.toList
    member __.Delay (f) =
        f()
    member __.Combine (t1 : Timeline<'t> list, t2) =
        t1 @ t2
    member this.For (i : 'a seq, f : 'a -> Timeline<'t> list) =
        i |> Seq.collect f |> this.YieldFrom
    member __.Run (timelines : Timeline<'t> list) =
        Animation (timelines)

let animation<'t when 't : comparison> = AnimationBuilder<'t>()
let animationSingle (tl : Timeline<'t>) = animation.Yield (tl) |> animation.Run

type IAnimationValueProvider<'t when 't : comparison> =
    abstract member Item : 't -> float
    abstract member Function : 't -> (float -> float)

type Scene<'t, 'r when 't : comparison> =
    { EnterAnimation : Animation<'t>
      RunAnimation : Animation<'t>
      LeaveAnimation : Animation<'t>
      Render : 'r -> IAnimationValueProvider<'t> -> float -> unit }

type SceneBuilder<'t, 'r when 't : comparison>() =
    let zeroAnimation = animation.Zero () |> animation<'t>.Run
    member __.Zero () = 
        { EnterAnimation = zeroAnimation
          RunAnimation = zeroAnimation
          LeaveAnimation = zeroAnimation
          Render = fun _ _ _ -> () }
    member this.Yield (_) =
        this.Zero ()
    [<CustomOperation("enter")>]
    member __.EnterAnimation (scene, enter : Animation<'t>) =
        if enter.Duration |> isInfinite then
            console.warn (
                "Scene with infinite enter animation detected. Please use the run animation",
                "for repeating loops. Enter should only be used to transition into the scene.")
        { scene with EnterAnimation = enter }
    member this.EnterAnimation (scene, enter : Timeline<'t>) =
        this.EnterAnimation (scene, animationSingle enter)
    [<CustomOperation("run")>]
    member __.RunAnimation (scene, run : Animation<'t>) =
        { scene with RunAnimation = run }
    member this.RunAnimation (scene, run : Timeline<'t>) =
        this.RunAnimation (scene, animationSingle run)
    [<CustomOperation("leave")>]
    member __.LeaveAnimation (scene, leave : Animation<'t>) =
        if leave.Duration |> isInfinite then
            console.warn (
                "Scene with infinite leave animation detected. Please use the run animation",
                "for repeating loops. Leave should only be used to transition out of the scene.")
        { scene with LeaveAnimation = leave }
    member this.LeaveAnimation (scene, leave : Timeline<'t>) =
        this.LeaveAnimation (scene, animationSingle leave)
    [<CustomOperation("render")>]
    member __.Render (scene, render) =
        { scene with Render = render }
    member this.Render (scene, render : 'r -> IAnimationValueProvider<'t> -> unit) =
        this.Render (scene, fun r a _ -> render r a)
    member this.Render (scene, render : 'r -> unit) =
        this.Render (scene, fun r _ _ -> render r)
    member __.Run (scene) =
        let initials (anim : Animation<'t>) =
            anim.Variables
            |> List.map (fun var -> var, anim.[var] (anim.Duration |> singleDuration))
            |> Map.ofList
        let reverseInitials vars (anim : Animation<'t>) =
            vars
            |> Seq.map (fun var -> var, anim.[var] 0.)
            |> Map.ofSeq
        let runAnimation = scene.RunAnimation.WithInitials (initials scene.EnterAnimation)
        let leaveAnimation = scene.LeaveAnimation.WithInitials (initials runAnimation)
        let leaveRunDiff = (set leaveAnimation.Variables) - (set runAnimation.Variables)
        let runAnimation = runAnimation.WithInitials (reverseInitials leaveRunDiff leaveAnimation)
        let runEnterDiff = (set runAnimation.Variables) - (set scene.EnterAnimation.Variables)
        let enterAnimation = scene.EnterAnimation.WithInitials (reverseInitials runEnterDiff runAnimation)
        { scene with
            EnterAnimation = enterAnimation
            RunAnimation = runAnimation
            LeaveAnimation = leaveAnimation }

let scene<'t, 'r when 't : comparison> = SceneBuilder<'t, 'r>()

module Scene =
    let getAnimationRenderFunction r anim scene =
        fun t ->
            scene.Render r ({ 
                new IAnimationValueProvider<'t> with 
                    member __.Item (var) = anim t var
                    member __.Function (var) = fun t -> anim t var
            }) t

    let getRenderFunction r scene =
        let enterDuration = scene.EnterAnimation.Duration |> singleDuration
        let runDuration = scene.RunAnimation.Duration |> singleDuration
        let anim t =
            if t < enterDuration 
            then fun var -> scene.EnterAnimation.[var] t
            elif t >= enterDuration && (t < enterDuration + runDuration || isInfinite scene.RunAnimation.Duration) 
            then fun var -> scene.RunAnimation.[var] (t - enterDuration)
            else fun var -> scene.LeaveAnimation.[var] (t - enterDuration - runDuration)
        getAnimationRenderFunction r anim scene
        
    let getEnterRenderFunction r scene =
        getAnimationRenderFunction r (fun t var -> scene.EnterAnimation.[var] t) scene
    let getRunRenderFunction r scene =
        getAnimationRenderFunction r (fun t var -> scene.RunAnimation.[var] t) scene
    let getLeaveRenderFunction r scene =
        getAnimationRenderFunction r (fun t var -> scene.LeaveAnimation.[var] t) scene

    let runAnimationLoopRender render =
        let mutable start = 0.
        let rec run render gt = 
            render (if start = 0. then start <- gt; 0. else gt - start)
            window.requestAnimationFrame (run render) |> ignore
        window.requestAnimationFrame (run render) |> ignore
       
    let runAnimationLoop r scene =
        let render = getRenderFunction r scene
        runAnimationLoopRender render

    let withRender render scene =
        { scene with Render = fun r tl t -> render r tl t scene.Render }

module Preview =
    open Browser.Types
    open Elmish
    open Elmish.React
    open Fable.React.Helpers
    open Fable.React.Props
    open Fable.React.Standard

    type SceneStage = Enter | Run | Leave
    type Playing = Playing of lastFrameTime : float | Paused

    type PreviewState<'t, 'r when 't : comparison> =
        { CurrentScene : Scene<'t, 'r>
          CurrentSceneIndex : int
          SceneStage : SceneStage
          Time : float
          Playing : Playing }

    let encodePreviewState state =
        Encode.object [
            "currentSceneIndex", Encode.int state.CurrentSceneIndex
            "sceneStage", Encode.Auto.generateEncoder<SceneStage>() state.SceneStage
            "time", Encode.float state.Time
        ]

    let decodePreviewState (scenes : Scene<'t, 'r> list)=
        Decode.object <| fun get ->
            let index = get.Required.Field "currentSceneIndex" Decode.int
            { CurrentScene = scenes.[index]
              CurrentSceneIndex = index
              SceneStage = get.Required.Field "sceneStage" (Decode.Auto.generateDecoder<SceneStage>())
              Time = get.Required.Field "time" Decode.float
              Playing = Paused }
       
    type PreviewMessage<'t, 'r when 't : comparison> =
        | SetScene of int * Scene<'t, 'r>
        | SetStage of SceneStage
        | SetTime of float
        | AnimationFrameGranted of time : float
        | StartPlaying
        | PausePlaying

    let withHashState program =
        let urlUpdate update msg model =
            let (newModel, cmd) = update msg model
            let hash = Encode.toString 0 (encodePreviewState newModel)
            if newModel.Playing = Paused then
                history.replaceState ((), url = "#" + window.btoa hash)
            newModel, Cmd.batch [ cmd; ]
        Program.map id urlUpdate id id id program

    let runPreview renderingCtx appDivId scenes =
        let renderScene model =
            let render =
                match model.SceneStage with
                | Enter -> Scene.getEnterRenderFunction
                | Run -> Scene.getRunRenderFunction
                | Leave -> Scene.getLeaveRenderFunction
            render renderingCtx model.CurrentScene model.Time
        let maxTime model =
            match model.SceneStage with
            | Enter -> model.CurrentScene.EnterAnimation.Duration |> singleDuration
            | Run -> model.CurrentScene.RunAnimation.Duration |> singleDuration
            | Leave -> model.CurrentScene.LeaveAnimation.Duration |> singleDuration
        let requestAnimationFrameCmd = Cmd.ofSub (fun dispatch ->
            window.requestAnimationFrame (fun t ->
                dispatch (AnimationFrameGranted t)
            ) |> ignore
        )
        let defaultState = 
            { CurrentScene = scenes |> List.head
              CurrentSceneIndex = 0
              SceneStage = Enter
              Time = 0.
              Playing = Paused }, Cmd.none
        let init () = 
            let hash = window.location.hash.TrimStart '#' |> window.atob
            if String.IsNullOrWhiteSpace hash then 
                defaultState
            else 
                match Decode.fromString (decodePreviewState scenes) hash with
                | Ok state -> 
                    state, Cmd.none
                | Error msg ->
                    console.error ("Error parsing state hash:", msg)
                    defaultState
        let update msg model =
            match msg with
            | SetScene (i, scene) -> 
                { model with CurrentScene = scene; CurrentSceneIndex = i; Time = 0. }, Cmd.none
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
        let view model dispatch =
            do renderScene model
            let dispatchButton message value style =
                input [ 
                    Type "button"
                    OnClick (fun _ -> dispatch message)
                    Value value
                    Style style
                ]
            div [] [
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
                        dispatchButton (SetScene (i, scene)) i [ if model.CurrentSceneIndex = i then FontWeight "bold" ]
                ]
            ]
        Program.mkProgram init update view
        |> withHashState
        |> Program.withReactBatched appDivId
        |> Program.run
