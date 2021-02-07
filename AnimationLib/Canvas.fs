module Canvas

open System
open Fable.Core
open Fable.Core.JsInterop
open Browser.Types

type Style = U3<string,CanvasGradient,CanvasPattern>

let inline rgb (r, g, b) : Style = 
    U3.Case1 (sprintf "rgb(%f, %f, %f)" (float r) (float g) (float b))
let inline rgba (r, g, b, a) : Style = 
    U3.Case1 (sprintf "rgba(%f, %f, %f, %f)" (float r) (float g) (float b) a)
let inline color (str : string) : Style = 
    U3.Case1 str

let style style s = (Some style, None, s)
let font font s = (None, Some font, s)
let styleFont style font s = (Some style, Some font, s)

let quadraticControlPoint fromX fromY toX toY cpDist =
    let center = fromX + (toX - fromX) / 2., fromY + (toY - fromY) / 2.
    let perp = -(toY - fromY), (toX - fromX)
    let perpl = sqrt((fst perp)**2. + (snd perp)**2.)
    let perp = (fst perp / perpl), (snd perp / perpl)
    (fst center) + cpDist * (fst perp), (snd center) + cpDist * (snd perp)

let private lerp x y t = (1.0 - t) * x + t * y

let private quadraticProgress progress =
    let t00 = progress**2.
    let t01 = 1. - progress
    let t02 = t01**2.
    let t03 = 2. * progress * t01
    fun start control end' -> t02 * start + t03 * control + t00 * end'

// https://www.pjgalbraith.com/drawing-animated-curves-javascript/
let quadraticProgressPoints fromX fromY cX cY toX toY progressStart progressEnd=
    if progressStart = 0. && progressEnd = 1. then
        (fromX, fromY, cX, cY, toX, toY)
    elif progressStart = progressEnd then
        (fromX, fromY, fromX, fromY, fromX, fromY)
    else
        let ps = quadraticProgress progressStart
        let nToX = ps fromX cX toX
        let nToY = ps fromY cY toY
        let pe = quadraticProgress progressEnd
        let nFromX = pe fromX cX toX
        let nFromY = pe fromY cY toY
        let nCX = lerp (lerp fromX cX progressStart) (lerp cX toX progressStart) progressEnd
        let nCY = lerp (lerp fromY cY progressStart) (lerp cY toY progressStart) progressEnd
        (nFromX, nFromY, nCX, nCY, nToX, nToY)

let bezierLineSegments segments fromX fromY cX cY toX toY =
    let d = 1. / float segments
    [ for i in 0. .. d .. (1. - d) -> 
        let (fromX, fromY, _, _, toX, toY) = quadraticProgressPoints fromX fromY cX cY toX toY i (i + d)
        (fromX, fromY), (toX, toY) ]

let private limit bottom top = max bottom >> min top

let staggeredProgress stagger length index progress = 
    if length >= 2
    then limit 0. 1. ((stagger + 1.) * progress - (float index * stagger / float (length - 1)))
    else progress

let stagger total index progress =
    staggeredProgress 1. total index progress

let circlePoint cX cY radius angle =
    let x = radius * cos(angle) + cX
    let y = radius * sin(angle) + cY
    (x, y)

type ExtendedTextMetrics =
    inherit TextMetrics
    abstract actualBoundingBoxAscent : float with get, set
    abstract actualBoundingBoxDescent : float with get, set
    abstract fontBoundingBoxAscent : float with get, set
    abstract fontBoundingBoxDescent : float with get, set

module private CanvasContext =
    let mutable lineHeight = 1.2

type CanvasRenderingContext2D with
    member __.lineHeight with get () = CanvasContext.lineHeight
    member __.lineHeight with set (value) = CanvasContext.lineHeight <- value
    member ctx.width = ctx.canvas.width
    member ctx.height = ctx.canvas.height
    member ctx.actualLineHeight with get () = 
        let textHeight =
            let longText = String [|'0'..'z'|]
            let m = ctx.measureText(longText) :?> ExtendedTextMetrics
            // Disable this one to unify rendering across Firefox and Chromium
            // if not (isNullOrUndefined m.fontBoundingBoxAscent) 
            // then m.fontBoundingBoxAscent + m.fontBoundingBoxDescent
            if not (isNullOrUndefined m.actualBoundingBoxAscent)
            then m.actualBoundingBoxAscent + m.actualBoundingBoxDescent
            else ctx.measureText("GMX").width / 3.
        textHeight * ctx.lineHeight
    member ctx.setStyle (style) =
        ctx.strokeStyle <- style
        ctx.fillStyle <- style
    member ctx.style with get () = if ctx.strokeStyle = ctx.fillStyle then Some ctx.strokeStyle else None
    member ctx.style with set (value) = ctx.setStyle value
    member ctx.clear () =
        ctx.clearRect (0., 0., ctx.width, ctx.height)
    member ctx.background (?color) =
        let color = defaultArg color (rgb (255, 255, 255))
        let prevFill = ctx.fillStyle
        ctx.fillStyle <- color
        ctx.fillRect (0., 0., ctx.width, ctx.height)
        ctx.fillStyle <- prevFill
    member ctx.ellipse (x, y, radiusX, ?radiusY, ?rotation, ?startAngle, ?endAngle) =
        let radiusY = defaultArg radiusY radiusX
        let rotation = defaultArg rotation 0.
        let startAngle = defaultArg startAngle 0.
        let endAngle = defaultArg endAngle (2. *  Math.PI)
        ctx.ellipse (x, y, radiusX, radiusY, rotation, startAngle, endAngle, false)
    member ctx.square (x, y, size) =
        ctx.rect (x, y, size, size)
    member ctx.partialQuadraticCurve (fromX, fromY, cX, cY, toX, toY, progressStart, progressEnd) =
        let (fromX, fromY, cX, cY, toX, toY) = quadraticProgressPoints fromX fromY cX cY toX toY progressStart progressEnd
        ctx.moveTo (fromX, fromY)
        ctx.quadraticCurveTo (cX, cY, toX, toY)
    member ctx.curve (fromX, fromY, toX, toY, cpDist, ?progressStart, ?progressEnd) =
        let progressStart = defaultArg progressStart 0.
        let progressEnd = defaultArg progressEnd 1.
        let control = quadraticControlPoint fromX fromY toX toY cpDist
        ctx.partialQuadraticCurve (fromX, fromY, fst control, snd control, toX, toY, progressStart, progressEnd)
    // https://stackoverflow.com/questions/808826/draw-arrow-on-canvas-tag
    member ctx.arrow (fromX, fromY, toX, toY, ?cpDist, ?progressStart, ?progressEnd, ?headSize) =
        let cpDist = defaultArg cpDist 0.
        let headSize = defaultArg headSize (10. + 2. * ctx.lineWidth)
        let progressStart = defaultArg progressStart 1.
        let progressEnd = defaultArg progressEnd 0.
        if progressStart <> progressEnd then
            let (cX, cY) = quadraticControlPoint fromX fromY toX toY cpDist
            let (fromX, fromY, cX, cY, toX, toY) = quadraticProgressPoints fromX fromY cX cY toX toY progressStart progressEnd
            let returnAngle = 0.5 * Math.PI - atan2 (toX - cX) (toY - cY)
            let pi6 = Math.PI / 6.
            ctx.save ()
            ctx.beginPath ()
            ctx.lineJoin <- "bevel"
            ctx.moveTo (fromX, fromY)
            ctx.quadraticCurveTo (cX, cY, toX, toY)
            ctx.lineTo (toX - headSize * cos(returnAngle - pi6), toY - headSize * sin(returnAngle - pi6))
            ctx.moveTo (toX - headSize * cos(returnAngle + pi6), toY - headSize * sin(returnAngle + pi6))
            ctx.lineTo (toX, toY)
            ctx.stroke ()
            ctx.closePath ()
            ctx.restore ()
    member ctx.arcArrow (cX, cY, radius, startAngle, endAngle, ?anticlockwise, ?headSize) =
        if startAngle <> endAngle then
            let anticlockwise = defaultArg anticlockwise false
            let headSize = defaultArg headSize (10. + 2. * ctx.lineWidth)
            let returnAngle = endAngle + 0.5 * Math.PI * if anticlockwise then -1. else 1.
            let pi6 = Math.PI / 6.
            let toX = cX + radius * cos(endAngle)
            let toY = cY + radius * sin(endAngle)
            ctx.save ()
            ctx.beginPath ()
            ctx.lineJoin <- "bevel"
            ctx.arc (cX, cY, radius, startAngle, endAngle, anticlockwise)
            ctx.moveTo (toX, toY)
            ctx.lineTo (toX - headSize * cos(returnAngle - pi6), toY - headSize * sin(returnAngle - pi6))
            ctx.moveTo (toX - headSize * cos(returnAngle + pi6), toY - headSize * sin(returnAngle + pi6))
            ctx.lineTo (toX, toY)
            ctx.stroke ()
            ctx.closePath ()
            ctx.restore ()
    member ctx.drawText (textProgress : (char * float) seq, x, y) =
        let dashLen = 180.
        let text = textProgress |> Seq.map fst |> Seq.toArray |> String
        for (i, (ch, progress)) in textProgress |> Seq.indexed do
            let ch = string ch
            let x = x + ctx.measureText(text.[0..i-1]).width
            ctx.save ()
            let alpha = ctx.globalAlpha
            if progress < 1. then
                ctx.globalAlpha <- alpha * (1. - (progress - 0.8) * 5.)
                ctx.setLineDash [| progress * dashLen; (1. - progress) * dashLen |]
                ctx.strokeText(ch, x, y)
            if progress > 0.6 then
                ctx.globalAlpha <- alpha * (progress - 0.6) * 2.5
                ctx.fillText(ch, x, y)
            ctx.restore ()
    member ctx.drawText (text : string, x, y, progress, ?stagger) =
        let stagger = defaultArg stagger 0.5
        let textProgress =
            text
            |> Seq.indexed
            |> Seq.map (fun (i, ch) -> ch, staggeredProgress stagger text.Length i progress)
        ctx.drawText (textProgress, x, y)
    member ctx.drawLongText (text : (Style option * string option * string) list list, x, y, ?progress, ?stagger) = 
        let progress = defaultArg progress 1.
        let stagger = defaultArg stagger 0.5
        let charCountPerBlock = text |> List.map (List.map (fun (_, _, s) -> String.length s))
        let charCountPerLine = charCountPerBlock |> List.map List.sum
        let charCountTotal = charCountPerLine |> List.sum
        for li, line in text |> List.indexed do
            let y = y + float li * ctx.actualLineHeight
            let textSizes = 
                line 
                |> List.map (fun (_, font, str) -> 
                    font |> Option.iter (fun font -> ctx.font <- font)
                    ctx.measureText(str).width
                )
            let lineStartIndex = charCountPerLine.[0..li-1] |> List.sum
            for i in 0 .. line.Length - 1 do
                let (style, font, text) = line.[i]
                let x = x + (textSizes.[0..i-1] |> List.sum)
                let blockStartIndex = lineStartIndex + (charCountPerBlock.[li].[0..i-1] |> List.sum)
                let textProgress =
                    text
                    |> Seq.indexed
                    |> Seq.map (fun (i, ch) -> ch, staggeredProgress stagger charCountTotal (blockStartIndex + i) progress)
                ctx.save ()
                style |> Option.iter ctx.setStyle
                font |> Option.iter (fun font -> ctx.font <- font)
                ctx.drawText (textProgress, x, y)
                ctx.restore ()
    member ctx.fluffyCircle (cX, cY, radius, ?fluffSize, ?seed) =
        let fluffSize = defaultArg fluffSize (radius / 3.)
        let seed = defaultArg seed 1.1
        let circumference = 2. * Math.PI * radius
        let arcCount = round (circumference / fluffSize)
        let arcCircCovers = 
            let random = Seq.init (int arcCount) (fun i -> (Perlin.noise seed (float i * 50.)) * 2. + 1.)
            let sum = random |> Seq.sum
            random
            |> Seq.map (fun i -> i / sum * circumference)
            |> Seq.mapFold (fun start size -> ((start / radius, size / radius), start + size)) 0.  
            |> fst
        ctx.moveTo (cX + radius, cY)
        for (startAngle, coversAngle) in arcCircCovers do
            let halfAngle = 0.5 * coversAngle
            let arcX, arcY = circlePoint cX cY radius (startAngle + halfAngle)
            let arcRadius = cos((Math.PI - halfAngle) / 2.) * 2. * radius

            let arcBX, arcBY = circlePoint cX cY radius startAngle
            let beginAngle = 0.5 * Math.PI - atan2 (arcBX - arcX) (arcBY - arcY)
            let arcEX, arcEY = circlePoint cX cY radius (startAngle + coversAngle)
            let endAngle = 0.5 * Math.PI - atan2 (arcEX - arcX) (arcEY - arcY)

            ctx.arc (arcX, arcY, arcRadius, beginAngle, endAngle)            
    member ctx.fluffyEllipse (x, y, radiusX, radiusY, ?rotation, ?fluffSize, ?seed) =
        ctx.save ()
        ctx.translate (x, y)
        if radiusX < radiusY then ctx.scale (1., radiusY / radiusX)
        elif radiusY < radiusX then ctx.scale (radiusX / radiusY, 1.)
        ctx.rotate (defaultArg rotation 0.)
        ctx.fluffyCircle (0., 0., min radiusX radiusY, ?fluffSize = fluffSize, ?seed = seed)
        ctx.restore ()
