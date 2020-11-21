module Animation

open Browser.Dom

type Easing = 
    | Linear 
    | Custom of (float -> float)

let private easingFunction = function
    | Linear -> fun x -> x
    | Custom f -> f

type Key = 
    { Value : float option
    //   duration : float option
    //   delay : float
      Easing : Easing }

type KeyBuilder() =
    member __.Yield _ = { Value = None; (*duration = None; delay = 0.;*) Easing = Linear }
    [<CustomOperation("value")>]
    member __.Value (key : Key, value) = { key with Value = Some value }
    member this.Value (key : Key, value : int) = this.Value (key, float value)
    // [<CustomOperation("duration")>]
    // member __.Duration (key : Key, duration) = { key with duration = Some duration }
    // member this.Duration (key : Key, duration : int) = this.Duration (key, float duration)
    // [<CustomOperation("delay")>]
    // member __.Delay' (key : Key, delay) = { key with delay = delay }
    // member this.Delay' (key : Key, delay : int) = this.Delay' (key, float delay)
    [<CustomOperation("easing")>]
    member __.Easing (key : Key, easing) = { key with Easing = easing }

let key = KeyBuilder()

let (=>) x y = x, y

type Var<'t> = 't * Key
type VarsBuilder<'t>() =
    member __.Zero () = []
    member __.Yield (var : Var<'t>) = [ var ]
    member this.Yield ((var, v) : 't * float) = this.Yield ((var, key { value v }))
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
      Direction : Direction
      Loop : Loop }

module Timeline =
    let delay timeDelay timeline =
        let ts = 
            timeline.Timestamps 
            |> List.map (fun (t, vars) -> t + timeDelay, vars)
        { timeline with Timestamps = ts }

let private calculateTimeline timeline =
    let ts = timeline.Timestamps |> List.sortBy fst
    let totalDur = ts |> List.last |> fst
    let chooseKeys var (t, vars) = 
        vars 
        |> List.tryFind (fun (v, _) -> v = var) 
        |> Option.map (fun (_, key) -> (t, key))
    let getKeys var = 
        let keys = ts |> List.choose (chooseKeys var)
        keys 
        |> List.mapFold (fun (startTime, prev) (endTime, key) ->
            let value = key.Value |> Option.defaultValue prev
            let valueFunc t = 
                let duration = endTime - startTime
                let progression = (t - startTime) / duration
                let easedProgression = easingFunction key.Easing progression
                prev + (value - prev) * easedProgression
            {| StartTime = startTime; EndTime = endTime
               Value = value; ValueFunc = valueFunc |},
            (endTime, value)) (0., (snd keys.Head).Value |> Option.defaultValue 0.)
        |> fst
    ts |> List.collect snd |> List.map fst |> List.distinct
    |> List.map (fun var -> var, getKeys var)
    |> Map.ofList
    |> Map.map (fun _ keys ->    
        fun t -> 
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

type CalculatedTimeline<'t when 't : comparison>(timeline : Timeline<'t>) =
    let animationFunctions = calculateTimeline timeline
    member __.Timeline = timeline
    member __.Item (var) = animationFunctions.[var]

type TimelineBuilder(?direction, ?loop) =
    let direction = defaultArg direction Normal
    let loop = defaultArg loop (Repeat 1)

    member __.Zero () = 
        { Timestamps = []; Direction = direction; Loop = loop }
    member __.Yield (ts) = 
        { Timestamps = [ ts ]; Direction = direction; Loop = loop }
    member this.Yield ((t, var) : int * Var<'t> list) = 
        this.Yield ((float t, var))
    member __.Delay (f) = 
        f()
    member __.Combine (t1, t2) = 
        { t1 with Timestamps = t1.Timestamps @ t2.Timestamps }
    member __.Run (timeline) = 
        CalculatedTimeline(timeline)

let timeline = TimelineBuilder()
let timeline' (direction, loop) = TimelineBuilder (direction, loop)

let runAnimation render =
    let mutable start = 0.
    let rec run render gt = 
        render (if start = 0. then start <- gt; 0. else gt - start)
        window.requestAnimationFrame (run render) |> ignore
    window.requestAnimationFrame (run render) |> ignore