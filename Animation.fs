module Animation

open System
open Browser.Dom

module List =
    let duplicates list =
        let (duplicates, _) =
            List.mapFold (fun found elem -> 
                if found |> Set.contains elem 
                then Some elem, found 
                else None, found |> Set.add elem
            ) (Set.empty) list
        duplicates |> List.choose id

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
    let n1 = 7.5625
    let d1 = 2.75
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
            let valueFunc t = 
                let duration = endTime - startTime
                let progression = (t - startTime) / duration
                let easedProgression = easingFunction key.Easing progression
                prev + (key.Value - prev) * easedProgression
            {| StartTime = startTime; EndTime = endTime
               Value = key.Value; ValueFunc = valueFunc |},
            (endTime, key.Value)) (0., (snd keys.Head).Value)
        |> fst
    ts |> List.collect snd |> List.map fst |> List.distinct
    |> List.map (fun var -> var, getKeys var)
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

type Animation<'t when 't : comparison>(timelines : Timeline<'t> list) =
    let animationFunctions = 
        timelines 
        |> List.collect (calculateTimeline >> Map.toList)
    do for var in animationFunctions |> List.map fst |> List.duplicates do
        console.warn (
            "Variable", var, "is defined in multiple parallel timelines.",
            "Only the definition in the first timeline will be used.")
    let functionsMap = 
        animationFunctions 
        |> List.distinctBy fst 
        |> Map.ofList
    member __.Timelines = timelines
    member __.Item (var) = functionsMap.[var]

type AnimationBuilder() =
    member __.Zero () =
        []
    member __.Yield ((delay, timeline) : float * Timeline<'t>) =
        [ { timeline with Delay = delay } ]
    member this.Yield ((delay, timeline) : int * Timeline<'t>) =
        this.Yield ((float delay, timeline))
    member __.Yield (timeline : Timeline<'t>) =
        [ timeline ]
    member __.Delay (f) =
        f()
    member __.Combine (t1 : Timeline<'t> list, t2) =
        t1 @ t2
    member __.Run (timelines) =
        Animation (timelines)

let animation = AnimationBuilder()
let animationSingle (tl : Timeline<'t>) = animation.Yield (tl) |> animation.Run

let runAnimation render =
    let mutable start = 0.
    let rec run render gt = 
        render (if start = 0. then start <- gt; 0. else gt - start)
        window.requestAnimationFrame (run render) |> ignore
    window.requestAnimationFrame (run render) |> ignore