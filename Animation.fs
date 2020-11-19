module Animation

open Browser.Dom

type Easing = 
    | Linear 
    | Custom of (float -> float)
type Key = 
    { value : float option
      duration : float option
      delay : float
      easing : Easing }

type KeyBuilder() =
    member __.Yield _ = { value = None; duration = None; delay = 0.; easing = Linear }
    [<CustomOperation("value")>]
    member __.Value (key : Key, value) = { key with value = Some value }
    member this.Value (key : Key, value : int) = this.Value (key, float value)
    [<CustomOperation("duration")>]
    member __.Duration (key : Key, duration) = { key with duration = Some duration }
    member this.Duration (key : Key, duration : int) = this.Duration (key, float duration)
    [<CustomOperation("delay")>]
    member __.Delay' (key : Key, delay) = { key with delay = delay }
    member this.Delay' (key : Key, delay : int) = this.Delay' (key, float delay)
    [<CustomOperation("easing")>]
    member __.Easing (key : Key, easing) = { key with easing = easing }

let key = KeyBuilder()

let private easingFunction = function
    | Linear -> fun x -> x
    | Custom f -> f

type AnimationDirection = 
    | Normal 
    | Reverse 
    | Alternate
type AnimationLoop = 
    | Repeat of int 
    | Infinite
type Animation = 
    { start : float
      keys : Key list
      duration : float option
      delay : float
      direction : AnimationDirection
      loop : AnimationLoop }

type AnimationBuilder (start: float) =
    member __.Yield _ = { start = start; keys = []; duration = None; delay = 0.; direction = Normal; loop = Repeat 1 }
    [<CustomOperation("keys")>]
    member __.Keys (animation : Animation, keys) = { animation with keys = keys }
    [<CustomOperation("duration")>]
    member __.Duration (animation : Animation, duration) = { animation with duration = Some duration }
    member this.Duration (animation : Animation, duration : int) = this.Duration (animation, float duration)
    [<CustomOperation("delay")>]
    member __.Delay' (animation : Animation, delay) = { animation with delay = delay }
    member this.Delay' (animation : Animation, delay : int) = this.Delay' (animation, float delay)
    [<CustomOperation("direction")>]
    member __.Direction (animation : Animation, direction) = { animation with direction = direction }
    [<CustomOperation("loop")>]
    member __.Loop (animation : Animation, loop) = { animation with loop = loop }

    member __.Run (animation : Animation) =
        let totalTimeCovered = 
            animation.keys |> List.sumBy (fun key -> key.delay + (key.duration |> Option.defaultValue 0.))
        let unsetKeys = animation.keys |> List.filter (fun key -> key.duration = None) |> List.length

        let totalDur =
            match animation.duration with
            | Some dur -> 
                if totalTimeCovered > dur then
                    console.warn("Keyframes starting at value", animation.start, "specify a longer duration than",
                        "the total animation. Animation duration will be ignored.")
                    totalTimeCovered
                else dur
            | None ->
                if unsetKeys = 0 then 
                    console.warn("Animation starting at value", animation.start, 
                        "does not have a global duration, nor durations set for all keyframes.",
                        "Keyframes without a duration will be reduced to 0.")
                totalTimeCovered

        let starDuration = if unsetKeys > 0 then (totalDur - totalTimeCovered) / float unsetKeys else 0.

        let keyframes =
            animation.keys 
            |> List.map (fun key -> 
                {| value = key.value
                   duration = key.duration |> Option.defaultValue starDuration
                   delay = key.delay
                   easing = easingFunction key.easing |})
            |> List.mapFold (fun (t, prevVal) key ->
                    {| key with startTime = t + key.delay; prev = prevVal |}, 
                    (t + key.delay + key.duration, key.value |> Option.defaultValue prevVal)
                ) (0., animation.start)
            |> fst
            |> List.map (fun key -> 
                {| startTime = key.startTime 
                   endTime = key.startTime + key.duration
                   value = fun t -> 
                    key.prev + (t - key.startTime) * 
                    (((key.value |> Option.defaultValue key.prev) - key.prev) / key.duration) 
                |})

        fun t -> 
            let t =
                match animation.direction, animation.loop with
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
            let key = keyframes |> List.tryFind (fun key -> key.startTime <= t && t < key.endTime)
            match key with
            | Some key ->   
                key.value t
            | None when t < keyframes.Head.startTime ->
                animation.start
            | None ->
                let key = keyframes |> List.findBack (fun key -> t >= key.endTime)
                key.value key.endTime

let inline animation start = AnimationBuilder (float start)

let runAnimation render =
    let mutable start = 0.
    let rec run render gt = 
        render (if start = 0. then start <- gt; 0. else gt - start)
        window.requestAnimationFrame (run render) |> ignore
    window.requestAnimationFrame (run render) |> ignore