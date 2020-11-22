module Canvas

open System
open Fable.Core
open Browser.Types

let inline rgb (r, g, b) = U3.Case1 (sprintf "rgb(%f, %f, %f)" (float r) (float g) (float b))
let inline rgba (r, g, b, a) = U3.Case1 (sprintf "rgba(%f, %f, %f, %f)" (float r) (float g) (float b) a)
let inline color (str : string) = U3.Case1 str

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

type CanvasRenderingContext2D with
    member ctx.width = ctx.canvas.width
    member ctx.height = ctx.canvas.height
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
