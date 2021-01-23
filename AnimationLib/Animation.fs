module Animation.Animation

open System
open Browser.Dom

module Math =
    let rec gcd x y = if y = 0. then abs x else gcd y (x % y)
    let lcm x y = x * y / (gcd x y)

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
    let keys map =
        map |> Map.toList |> List.map fst

[<CustomEquality; NoComparison>]
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

    interface IEquatable<Easing> with
        member this.Equals (other) =
            match this, other with
            | Custom _, _ -> false
            | _, Custom _ -> false
            | _ -> this.ToString () = other.ToString ()

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

module Timestamps =
    let duration (ts : Timestamp<'t> list) =
        ts |> List.map fst |> List.maxOrDefault 0.

    let delay delay (ts : Timestamp<'t> list) : Timestamp<'t> list =
        if ts |> List.isEmpty
        then [ delay, [] ]
        else ts |> List.map (fun (t, vars) -> (t + delay, vars))

    let repeat times (ts : Timestamp<'t> list) : Timestamp<'t> list =
        let dur = duration ts
        Seq.init times (fun i -> delay (float i * dur) ts) |> List.concat

    let rotate shift (ts : Timestamp<'t> list) : Timestamp<'t> list =
        let dur = duration ts
        ts |> List.map (fun (t, vars) -> ((t + shift) % dur, vars))

    let getVariables (ts : Timestamp<'t> list) =
        ts 
        |> List.collect snd
        |> List.map fst
        |> List.distinct

    let getVariablesWithInitials (initials : Map<'t, float>) (ts : Timestamp<'t> list) =
        getVariables ts
        |> List.append (initials |> Map.keys)
        |> List.distinct

    let getKeys (ts : Timestamp<'t> list) var =
        ts 
        |> List.choose (
            fun (t, vars) -> 
                vars 
                |> List.tryFind (fun (v, _) -> v = var) 
                |> Option.map (fun (_, key) -> t, key)
        )

    let private keysToFunction keys =
        let keys =
            keys
            |> List.sortBy fst
            |> List.mapFold (fun (startTime, prev) (endTime, key) ->
                let valueFunc t = 
                    let duration = endTime - startTime
                    let progression = (t - startTime) / duration
                    let easedProgression = easingFunction key.Easing progression
                    prev + (key.Value - prev) * easedProgression
                {| StartTime = startTime; EndTime = endTime
                   Value = key.Value; ValueFunc = valueFunc |}, (endTime, key.Value)
            ) (0., (snd keys.Head).Value)
            |> fst
        fun t ->
            let key = keys |> List.tryFind (fun key -> key.StartTime <= t && t < key.EndTime)
            match key with
            | Some key ->   
                key.ValueFunc t
            | None when t < keys.Head.StartTime || keys.Length = 1 ->
                keys.Head.Value
            | None ->
                let key = keys |> List.findBack (fun key -> t >= key.EndTime)
                key.Value

    let calculate (initials : Map<'t, float>) (ts : Timestamp<'t> list) =
        let initials = [ for KeyValue (var, value) in initials -> var, { Value = value; Easing = Linear } ]
        let ts =
            match ts |> List.tryFind (fun (t, _) -> t = 0.) with
            | Some (t, vars) ->
                let unset = initials |> List.filter (fun (v, _) -> not (vars |> List.exists (fun (v', _) -> v = v')))
                (t, unset @ vars)::(ts |> List.filter (fun (t, _) -> t <> 0.))
            | None -> 
                (0., initials)::ts
        ts
        |> getVariables
        |> List.map (fun var -> var, getKeys ts var |> keysToFunction)
        |> Map.ofList

    let truncate endTime (ts : Timestamp<'t> list) : Timestamp<'t> list =
        let funcs = lazy (calculate Map.empty ts)
        let (truncated, rest) = ts |> List.partition (fun (t, _) -> t <= endTime)
        let cutOffVariables = 
            rest 
            |> getVariables 
            // TODO: use custom easing to get the right part of the curve of the original easing
            |> List.map (fun var -> var, { Value = funcs.Value.[var] endTime; Easing = Linear })
        truncated @ [ endTime => cutOffVariables ]

    let combine (ts1 : Timestamp<'t> list) (ts2 : Timestamp<'t> list) : Timestamp<'t> list =
        ts1 @ ts2
        |> List.groupBy fst
        |> List.map (fun (t, ts) -> (t, ts |> List.collect snd))
        |> List.sortBy fst

type Direction = 
    | Normal 
    | Reverse 
    | Alternate
    
type Loop = 
    | Repeat of int 
    | Infinite

type Timeline<'t> =
    { Initial : Timestamp<'t> list
      Loop : Timestamp<'t> list }

module Timeline =
    let inline delay delay tl =
        { tl with Initial = Timestamps.delay delay tl.Initial }
    
    let initialDuration tl = 
        Timestamps.duration tl.Initial

    let loopDuration tl = 
        Timestamps.duration tl.Loop

    let getVariables tl =
        (tl.Initial @ tl.Loop) |> Timestamps.getVariables

    let combine t1 t2 =
        let i1, i2 = initialDuration t1, initialDuration t2
        if i1 = i2 then
            let initial = Timestamps.combine t1.Initial t2.Initial
            let l1, l2 = loopDuration t1, loopDuration t2
            let loopDur = if l1 = 0. || l2 = 0. then max l1 l2 else Math.lcm l1 l2
            let loop = 
                Timestamps.combine
                    (t1.Loop |> Timestamps.repeat (int (loopDur / l1)))
                    (t2.Loop |> Timestamps.repeat (int (loopDur / l2)))
            { Initial = initial; Loop = loop }
        else
            let (long, il), (short, is) =
                if i1 < i2
                then (t2, i2), (t1, i1)
                else (t1, i1), (t2, i2)
            let ll, ls = loopDuration long, loopDuration short
            let shortLoopRepeatInitial = if ls = 0. then 0 else int (ceil ((il - is) / ls))
            let shortLoopShift = if ls = 0. then 0. else (il - is) % ls
            let initial = 
                let shortInitialFill = short.Loop |> Timestamps.delay is |> Timestamps.repeat shortLoopRepeatInitial
                Timestamps.combine
                    long.Initial
                    (short.Initial |> Timestamps.combine shortInitialFill |> Timestamps.truncate il)
            let loopDur = if ll = 0. || ls = 0. then max ll ls else Math.lcm ll ls
            let loop =
                Timestamps.combine
                    (long.Loop |> Timestamps.repeat (int (loopDur / ll)))
                    (short.Loop |> Timestamps.rotate shortLoopShift |> Timestamps.repeat (int (loopDur / ls)))
            { Initial = initial; Loop = loop }

    let calculate (initials : Map<'t, float>) (tl : Timeline<'t>) =
        let initialDur = initialDuration tl
        let initialFuncs = Timestamps.calculate initials tl.Initial
        let loopInitials = 
            tl.Initial 
            |> Timestamps.getVariablesWithInitials initials
            |> List.map (fun var -> var, initialFuncs.[var] initialDur)
            |> Map.ofList
        let loopDur = loopDuration tl
        let loopFuncs = Timestamps.calculate loopInitials tl.Loop
        fun var t ->
            if t < initialDur then 
                match initialFuncs |> Map.tryFind var with
                | Some f -> f t
                | None -> loopFuncs.[var] 0.
            elif loopDur > 0. then
                let t = (t - initialDur) % loopDur
                loopFuncs.[var] t
            else
                initialFuncs.[var] initialDur

type TimelineBuilder(?direction, ?loop) =
    let direction = defaultArg direction Normal
    let loop = defaultArg loop (Repeat 1)

    member __.Zero () = 
        []
    member __.Yield (ts : Timestamp<'t>) = 
        [ ts ]
    member this.Yield ((t, var) : int * Var<'t> list) = 
        this.Yield ((float t, var))
    member __.YieldFrom (timestamps : #seq<Timestamp<'t>>) =
        timestamps |> Seq.toList
    member __.Delay (f) = 
        f()
    member __.Combine (t1 : Timestamp<'t> list, t2) = 
        Timestamps.combine t1 t2
    member this.For (i : 'a seq, f : 'a -> Timestamp<'t> list) =
        i |> Seq.collect f |> this.YieldFrom
    member __.Run (timestamps) = 
        let dur = Timestamps.duration timestamps
        let reverse = timestamps |> List.map (fun (t, var) -> (dur - t, var))
        let repeated =
            match direction with 
            | Normal -> Seq.initInfinite (fun _ -> timestamps)
            | Reverse -> Seq.initInfinite (fun _ -> reverse)
            | Alternate -> Seq.initInfinite (fun i -> if i % 2 = 0 then timestamps else reverse)
            |> Seq.mapi (fun i -> Timestamps.delay (float i * dur))
        match loop with
        | Repeat i -> 
            { Initial = repeated |> Seq.take i |> List.concat
              Loop = [] }
        | Infinite -> 
            let i = match direction with Normal -> 1 | Reverse -> 1 | Alternate -> 2
            { Initial = []
              Loop = repeated |> Seq.take i |> List.concat }

let timeline = TimelineBuilder()
let timeline' (direction, loop) = TimelineBuilder (direction, loop)

module Animation =
    type AnimationDuration = 
        | FixedDuration of float 
        | InfiniteDuration of initialBlock : float * loop : float

    let isInfinite = function
        | InfiniteDuration _ -> true
        | _ -> false

    let singleDuration = function
        | FixedDuration d -> d
        | InfiniteDuration (initial, loop) -> initial + loop

type Animation<'t when 't : comparison>(timeline : Timeline<'t>, initials : Map<'t, float>) =
    let animationFunction = lazy (timeline |> Timeline.calculate initials)

    new(timeline : Timeline<'t>) = Animation(timeline, Map.empty)
    member __.WithTimeline (tl) = Animation(Timeline.combine timeline tl, initials)
    member __.WithInitial (key, value) = Animation(timeline, initials |> Map.add key value)
    member __.WithInitials (i) = Animation(timeline, Map.combine i initials)

    member __.Timeline = 
        timeline
    member __.Variables = 
        timeline 
        |> Timeline.getVariables
        |> List.append (initials |> Map.toList |> List.map fst)
        |> List.distinct
    member val Duration = 
        let id, ld = Timeline.initialDuration timeline, Timeline.loopDuration timeline
        if ld = 0.
        then Animation.FixedDuration id
        else Animation.InfiniteDuration (id, ld)
    member __.Item (var) = 
        animationFunction.Value var

    interface IEquatable<Animation<'t>> with
        member this.Equals (other) =
            this.Timeline = other.Timeline

type AnimationBuilder<'t when 't : comparison>() =
    member __.Zero () =
        timeline.Zero () |> timeline.Run
    member __.Yield ((delay, timeline) : float * Timeline<'t>) =
        timeline |> Timeline.delay delay
    member this.Yield ((delay, timeline) : int * Timeline<'t>) =
        this.Yield ((float delay, timeline))
    member __.Yield (timeline : Timeline<'t>) =
        timeline
    member this.YieldFrom (timelines : #seq<Timeline<'t>>) =
        timelines |> Seq.fold (Timeline.combine) (this.Zero ())
    member __.Delay (f) =
        f()
    member __.Combine (t1 : Timeline<'t>, t2) =
        Timeline.combine t1 t2
    member this.For (i : 'a seq, f : 'a -> Timeline<'t>) =
        i |> Seq.map f |> this.YieldFrom
    member __.Run (timeline : Timeline<'t>) =
        Animation (timeline)

let animation<'t when 't : comparison> = AnimationBuilder<'t>()
let animationSingle (tl : Timeline<'t>) = animation.Yield (tl) |> animation.Run

type IAnimationValueProvider<'t when 't : comparison> =
    abstract member Item : 't -> float
    abstract member Function : 't -> (float -> float)

[<CustomEquality; NoComparison>]
type Scene<'t, 'r when 't : comparison> =
    { Title : string
      EnterAnimation : Animation<'t>
      RunAnimation : Animation<'t>
      LeaveAnimation : Animation<'t>
      Render : 'r -> IAnimationValueProvider<'t> -> float -> unit }
    
    interface IEquatable<Scene<'t, 'r>> with
        member this.Equals (other) =
            this.Title = other.Title
            && this.EnterAnimation = other.EnterAnimation
            && this.RunAnimation = other.RunAnimation
            && this.LeaveAnimation = other.LeaveAnimation

type SceneBuilder<'t, 'r when 't : comparison>(title : string) =
    let zeroAnimation = animation.Zero () |> animation<'t>.Run
    member __.Zero () = 
        { Title = title
          EnterAnimation = zeroAnimation
          RunAnimation = zeroAnimation
          LeaveAnimation = zeroAnimation
          Render = fun _ _ _ -> () }
    member this.Yield (_) =
        this.Zero ()
    [<CustomOperation("enter")>]
    member __.EnterAnimation (scene, enter : Animation<'t>) =
        if enter.Duration |> Animation.isInfinite then
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
        if leave.Duration |> Animation.isInfinite then
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
            |> List.map (fun var -> var, anim.[var] (anim.Duration |> Animation.singleDuration))
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

let scene<'t, 'r when 't : comparison> title = SceneBuilder<'t, 'r> (title)

module Scene =
    let getAnimationRenderFunction r anim scene =
        fun t ->
            scene.Render r ({ 
                new IAnimationValueProvider<'t> with 
                    member __.Item (var) = anim t var
                    member __.Function (var) = fun t -> anim t var
            }) t

    let getRenderFunction loop r scene =
        let enterDuration = scene.EnterAnimation.Duration |> Animation.singleDuration
        let runDuration = scene.RunAnimation.Duration |> Animation.singleDuration
        let anim t =
            if t < enterDuration 
            then fun var -> scene.EnterAnimation.[var] t
            elif t >= enterDuration && (t < enterDuration + runDuration || (loop && Animation.isInfinite scene.RunAnimation.Duration))
            then fun var -> scene.RunAnimation.[var] (t - enterDuration)
            else fun var -> scene.LeaveAnimation.[var] (t - enterDuration - runDuration)
        getAnimationRenderFunction r anim scene

    let singleDuration scene =
        Animation.singleDuration (scene.EnterAnimation.Duration)
        + Animation.singleDuration (scene.RunAnimation.Duration)
        + Animation.singleDuration (scene.LeaveAnimation.Duration)
    
    let private animFunc (anim : Animation<'t>) = fun t var -> anim.[var] t

    let getEnterRenderFunction r scene =
        getAnimationRenderFunction r (animFunc scene.EnterAnimation) scene
    let getRunRenderFunction r scene =
        getAnimationRenderFunction r (animFunc scene.RunAnimation) scene
    let getLeaveRenderFunction r scene =
        getAnimationRenderFunction r (animFunc scene.LeaveAnimation) scene

    let withRender render scene =
        { scene with Render = fun r tl t -> render r tl t scene.Render }

    let mapTitle f scene =
        { scene with Title = f scene.Title }

    let getSplitRenderFunctions scene =
        let splitRenderAnimation title (anim : Animation<'t>) =
            [ match anim.Duration with
              | Animation.FixedDuration dur ->
                  if dur > 0. then
                      title, (fun r -> getAnimationRenderFunction r (animFunc anim) scene), dur
              | Animation.InfiniteDuration (initial, loop) ->
                  if initial > 0. then 
                      $"%s{title}_initial", (fun r -> getAnimationRenderFunction r (animFunc anim) scene), initial
                  if loop > 0. then 
                      $"%s{title}_loop", (fun r -> getAnimationRenderFunction r (fun t -> animFunc anim (t - initial)) scene), loop
            ]
        [ yield! splitRenderAnimation "0_enter" scene.EnterAnimation
          yield! splitRenderAnimation "1_run_1" scene.RunAnimation
          yield! splitRenderAnimation "2_leave" scene.LeaveAnimation ]
