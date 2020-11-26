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

let private quadraticControlPoint fromX fromY toX toY cpDist =
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
let private quadraticProgressPoints fromX fromY cX cY toX toY progressStart progressEnd=
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

type ExtendedTextMetrics =
    inherit TextMetrics
    abstract actualBoundingBoxAscent : float with get, set
    abstract actualBoundingBoxDescent : float with get, set
    abstract fontBoundingBoxAscent : float with get, set
    abstract fontBoundingBoxDescent : float with get, set

type CanvasRenderingContext2D with
    member ctx.width = ctx.canvas.width
    member ctx.height = ctx.canvas.height
    member ctx.setStyle (style) =
        ctx.strokeStyle <- style
        ctx.fillStyle <- style
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
        let progressStart = defaultArg progressStart 0.
        let progressEnd = defaultArg progressEnd 1.
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
    member ctx.drawText (text : string, x, y, progressF : float -> float, t : float, ?duration) =
        let dashLen = 180.
        let duration = defaultArg duration (100. * float text.Length)
        let delay = duration / float text.Length

        for i in 0 .. text.Length - 1 do
            let ch = string text.[i]
            let x = x + ctx.measureText(text.[0..i-1]).width
            let progress = progressF (t - float i * delay)
            ctx.save ()
            if progress < 1. then
                ctx.globalAlpha <- 1. - (progress - 0.8) * 5.
                ctx.setLineDash [| progress * dashLen; (1. - progress) * dashLen |]
                ctx.strokeText(ch, x, y)
            if progress > 0.6 then
                ctx.globalAlpha <- (progress - 0.6) * 2.5
                ctx.fillText(ch, x, y)
            ctx.restore ()
    member ctx.currentLineHeight with get () = 
        let longText = String [|'0'..'z'|]
        let m = ctx.measureText(longText) :?> ExtendedTextMetrics
        if not (isNullOrUndefined m.fontBoundingBoxAscent) 
        then m.fontBoundingBoxAscent + m.fontBoundingBoxDescent
        elif not (isNullOrUndefined m.actualBoundingBoxAscent)
        then m.actualBoundingBoxAscent + m.actualBoundingBoxDescent
        else ctx.measureText("GMX").width / 3.
    member ctx.drawLongText (text : (Style * string) list list, x, y, progressF : float -> float, t : float, ?lineHeight, ?duration) = 
        let lineHeight = (defaultArg lineHeight 1.2) * ctx.currentLineHeight
        let charCount = text |> List.map (List.map (snd >> String.length))
        let totalChars = charCount |> List.map List.sum |> List.sum
        let duration = defaultArg duration (50. * float totalChars)
        let delay = duration / float totalChars
        for li, line in text |> List.indexed do
            let y = y + float li * lineHeight
            let textSizes = line |> List.map (fun (_, str) -> ctx.measureText(str).width)
            let lineDelay = delay * float (charCount.[0..li-1] |> List.map List.sum |> List.sum)
            for i in 0 .. line.Length - 1 do
                let (style, text) = line.[i]
                let x = x + (textSizes.[0..i-1] |> List.sum)
                let delay = delay * float (charCount.[li].[0..i-1] |> List.sum)
                ctx.save ()
                ctx.setStyle (style)
                ctx.drawText (text, x, y, progressF, t - lineDelay - delay, delay * float text.Length)
                ctx.restore ()
    member ctx.drawLongText (text : (Style * string) list list, x, y, ?lineHeight) =
        ctx.drawLongText (text, x, y, (fun _ -> 1.), 1., ?lineHeight = lineHeight)