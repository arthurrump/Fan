module CTP.Shared

open System
open Browser.Types
open Animation.Animation
open Canvas

[<AutoOpen>]
module CodeColors =
    // Based on VS Code DarkVS and Dark+ themes
    let text = color "#D4D4D4"
    let comment = color "#6A9955"
    let funcDecl = color "#DCDCAA"
    let typeDecl = color "#4EC980"
    let keyword = color "#569cd6"
    let control = color "#C586C0"
    let operator = color "#D4D4D4"
    let var = color "#9CDCFE"
    let const' = color "#4FC1FF"
    let stringLit = color "#CE9178"
    let customLit = color "#DCDCAA"
    let numberLit = color "#B5CEA8"
    let xmlBracket = color "#808080"

let serifFont size = sprintf "%ipx 'CMU Serif', serif" size
let mathFont size = sprintf "%ipx 'CMU Classical Serif', math" size
let codeFont size = sprintf "%ipx Consolas, monospace" size

let inline fadeIn duration easing property = timeline {
    0 => vars { property => 0. }
    (float duration) => vars { property => (1., easing) }
}

let inline fadeOut duration easing property = timeline {
    0 => vars { property => 1. }
    (float duration) => vars { property => (0., easing) }
}

let intro = scene "intro" {
    run (timeline {
        0 => vars {
            "slash" => 0
            "scale" => 6
        }
        1300 => vars {
            "slashX" => 0
        }
        1500 => vars {
            "slash" => (1, EaseOutSine)
            "scale" => (1, EaseInSine)
        }
        2100 => vars {
            "slashX" => (1, EaseInOutQuad)
        }
        2650 => vars {
            "slashX" => 1
        }
        3300 => vars {
            "slashX" => (-1.5, EaseInCubic)
        }
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.textBaseline <- "middle"
        ctx.setStyle comment

        let slashFont = "bold " + (codeFont 360)
        let textFont = codeFont 100

        ctx.translate (ctx.width / 2., ctx.height / 2.)
        ctx.scale (tl.["scale"], tl.["scale"])

        ctx.font <- slashFont
        let slashOffset = -ctx.measureText("//").width / 2.
        ctx.font <- textFont
        let textX = -ctx.measureText("Create intro").width

        ctx.font <- slashFont
        ctx.drawText ("//", slashOffset + tl.["slashX"] * (textX + slashOffset), 0., tl.["slash"], 0.2)

        ctx.save ()
        ctx.beginPath ()
        let clipX = tl.["slashX"] * (textX + slashOffset) - 0.5 * slashOffset
        let slashAngle = 0.404916
        ctx.moveTo (clipX - ctx.height / 4. * cos(slashAngle), ctx.height / 2.)
        ctx.lineTo (clipX + ctx.height / 4. * cos(slashAngle), -ctx.height / 2.)
        ctx.lineTo (ctx.width / 2., -ctx.height / 2.)
        ctx.lineTo (ctx.width / 2., ctx.height / 2.)
        ctx.closePath ()
        ctx.clip ()

        ctx.font <- textFont
        ctx.fillText ("TODO:", textX, -50. * 1.2)
        ctx.fillText ("Create intro", textX, 50. * 1.2)
        ctx.restore ()
    )
}

let endCredits = scene "endCredits" {
    run (animation {
        0 => fadeIn 750 Linear "name"
        500 => fadeIn 750 Linear "url"
        13000 => fadeOut 500 Linear "opacity" 
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.textBaseline <- "top"
        ctx.setStyle (color "#fff")
        ctx.globalAlpha <- tl.["opacity"]

        ctx.save ()
        ctx.translate ((2./3.) * ctx.width, (2./3.) * ctx.height)
        ctx.font <- codeFont 70
        ctx.drawText ("Arthur Rump", 0., 0., tl.["name"])
        ctx.font <- codeFont 32
        ctx.drawText ("https://arthurrump.com", 0., 1.1 * 70., tl.["url"])
        ctx.restore ()
    )
}

let v_general = [ intro; endCredits ]

let inline interpolate a0 a1 w =
    (float a1 - float a0) * w + float a0

type TextAlign = 
    | LeftAbove | CenterAbove | RightAbove
    | Left                    | Right
    | LeftUnder | CenterUnder | RightUnder

module private CanvasContext =
    let mutable nodeSize : float = 25.

type CanvasRenderingContext2D with
    member __.nodeSize with get () = CanvasContext.nodeSize
    member __.nodeSize with set (value) = CanvasContext.nodeSize <- value
    member ctx.node (name, x, y, align, opacity, ?fontSize) =
        let size = ctx.nodeSize
        let fontSize = defaultArg fontSize (int (74. * size / 25.))
        let margin = size + 10.
        ctx.save ()
        ctx.lineWidth <- 1.
        ctx.strokeStyle <- color "#fff"
        ctx.fillStyle <- rgba (255, 255, 255, opacity)
        ctx.beginPath ()
        ctx.ellipse (x, y, size, endAngle = opacity * 2. * Math.PI)
        ctx.stroke ()
        ctx.beginPath ()
        ctx.ellipse (x, y, size)
        ctx.fill ()

        ctx.setStyle (color "#fff")
        ctx.font <- serifFont fontSize
        let (textX, textY, baseline) =
            let width = ctx.measureText(name).width
            match align with
            | LeftAbove -> (x - width, y - margin, "bottom")
            | CenterAbove -> (x - width / 2., y - margin, "bottom")
            | RightAbove -> (x, y - margin, "bottom")
            | Left -> (x - width - margin, y, "middle")
            | Right -> (x + margin, y, "middle")
            | LeftUnder -> (x - width, y + margin, "top")
            | CenterUnder -> (x - width / 2., y + margin, "top")
            | RightUnder -> (x, y + margin, "top")
        ctx.textBaseline <- baseline
        ctx.drawText (name, textX, textY, opacity)
        ctx.restore ()
    member ctx.nodeArrow (fromX, fromY, toX, toY, cpDist, ?progressStart, ?progressEnd, ?headSize) =
        let progressStart = defaultArg progressStart 1.
        let progressEnd = defaultArg progressEnd 0.
        let (cX, cY) = quadraticControlPoint fromX fromY toX toY cpDist
        let arrowLength = 
            bezierLineSegments 5 fromX fromY cX cY toX toY
            |> List.map (fun ((bx, by), (ex, ey)) -> sqrt ((bx - ex)**2. + (by - ey)**2.))
            |> List.sum

        let nodeHiddenPart = (ctx.nodeSize + 5.) / arrowLength
        let progressStart = interpolate nodeHiddenPart (1. - nodeHiddenPart) progressStart
        let progressEnd = interpolate nodeHiddenPart (1. - nodeHiddenPart) progressEnd

        ctx.arrow (fromX, fromY, toX, toY, cpDist, progressStart, progressEnd, ?headSize = headSize)

let category = scene "test-category" {
    enter (animation {
        fadeIn 1000 Linear "ObjectVoid"
        fadeIn 1000 Linear "ObjectUnit"
        fadeIn 1000 Linear "ObjectBool"
        1000 => fadeIn 1000 EaseOutQuad "ArrowVoidUnit"
        1000 => fadeIn 1000 EaseOutQuad "ArrowVoidBool"
        2000 => fadeIn 1000 EaseOutQuad "ArrowUnitBool"
        3000 => fadeIn 1000 EaseOutQuad "ArrowBoolUnit"
        4000 => fadeIn 1000 EaseOutQuad "ArrowIdentity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.node ("Unit", 250., 700., LeftUnder, tl.["ObjectUnit"])
        ctx.node ("Bool", 800., 700., RightUnder, tl.["ObjectBool"])
        ctx.node ("Void", 525., 300., CenterAbove, tl.["ObjectVoid"])

        ctx.strokeStyle <- color "#fff"
        ctx.lineWidth <- 5.

        ctx.arrow (490., 300., 250., 665., 100., tl.["ArrowVoidUnit"])
        ctx.arrow (560., 300., 800., 665., -100., tl.["ArrowVoidBool"])
        ctx.arrow (285., 690., 765., 690., -50., tl.["ArrowUnitBool"])
        ctx.arrow (285., 690., 765., 690., -100., tl.["ArrowUnitBool"])
        ctx.arrow (765., 710., 285., 710., -50., tl.["ArrowBoolUnit"])

        ctx.arcArrow (835., 695., 25., 0.6 * Math.PI, (0.6 - 1.25 * tl.["ArrowIdentity"]) * Math.PI, true)
    )
}

let inline fromTo from to' duration easing property = timeline {
    0 => vars { property => (float from) }
    (float duration) => vars { property => (float to', easing) }
}

let drawLanguageIndicator lang progress (ctx : CanvasRenderingContext2D) =
    ctx.save ()
    ctx.setStyle (color "#fffc")
    ctx.lineWidth <- 1.
    ctx.font <- codeFont 60
    ctx.textBaseline <- "alphabetic"
    ctx.drawText (lang, 50., ctx.height - 50., progress)
    ctx.restore ()


let typeCloud x y radiusX radiusY seed (tl : IAnimationValueProvider<string>) (ctx : CanvasRenderingContext2D) =
    ctx.save ()
    ctx.fillStyle <- color "#fffa"
    ctx.strokeStyle <- color "#fffd"
    ctx.lineWidth <- 3.
    ctx.beginPath ()
    ctx.fluffyEllipse (x, y, radiusX, radiusY, seed = tl.["seed"] + seed)
    ctx.fill ()
    ctx.stroke ()
    ctx.restore ()

let setFuncStyle (ctx : CanvasRenderingContext2D) =
    ctx.setStyle (color "#fff")
    ctx.lineWidth <- 5.

let func startX startY endX endY curve progressIn progressOut (ctx : CanvasRenderingContext2D) =
    if progressIn <> progressOut then
        ctx.save ()
        ctx |> setFuncStyle
        ctx.arrow (startX, startY, endX, endY, curve, progressIn, progressOut)
        ctx.restore ()


type CanvasRenderingContext2D with
    member ctx.drawHighlighted (?highlightProgress, ?highlightColor) = fun f ->
        let highlightProgress = defaultArg highlightProgress 1.
        let highlightColor = defaultArg highlightColor "#0d4dff0f"
        if highlightProgress > 0. then
            ctx.save ()
            ctx.setStyle (color highlightColor)
            ctx.shadowColor <- highlightColor
            ctx.shadowBlur <- 1.
            let origLineWidth = ctx.lineWidth
            for i in 4.5 .. -0.25 .. 0. do
                ctx.lineWidth <- origLineWidth * (1. + i * highlightProgress)
                f ctx
            ctx.restore ()
        f ctx
    member ctx.highlightedArrow (fromX, fromY, toX, toY, ?cpDist, ?progressStart, ?highlightProgress, ?highlightColor, ?progressEnd, ?headSize) =
        let headSize = defaultArg headSize (10. + 2. * ctx.lineWidth)
        ctx.drawHighlighted (?highlightProgress = highlightProgress, ?highlightColor = highlightColor) <| fun ctx ->
            ctx.arrow (fromX, fromY, toX, toY, ?cpDist = cpDist, ?progressStart = progressStart, ?progressEnd = progressEnd, headSize = headSize)
    member ctx.highlightedArcArrow (cX, cY, radius, startAngle, endAngle, ?highlightProgress, ?highlightColor, ?anticlockwise, ?headSize) =
        let headSize = defaultArg headSize (10. + 2. * ctx.lineWidth)
        ctx.drawHighlighted (?highlightProgress = highlightProgress, ?highlightColor = highlightColor) <| fun ctx ->
            ctx.arcArrow (cX, cY, radius, startAngle, endAngle, ?anticlockwise = anticlockwise, headSize = headSize)

type CanvasRenderingContext2D with
    member ctx.blockArrow (pX, pY, width, height, ?progress, ?direction) =
        let progress = defaultArg progress 1.
        let direction = defaultArg direction 0.
        if progress > 0. then
            ctx.save ()
            ctx.translate (pX, pY + (1. - progress) * height)
            ctx.rotate (direction)
            ctx.moveTo (0., 0.)
            ctx.lineTo (-0.5 * width * progress, 0.5 * width * progress)
            ctx.moveTo (0., 0.)
            ctx.lineTo (0.5 * width * progress, 0.5 * width * progress)
            ctx.moveTo (-0.25 * width * progress, 0.25 * width * progress)
            ctx.lineTo (-0.25 * width, height * progress)
            ctx.moveTo (0.25 * width * progress, 0.25 * width * progress)
            ctx.lineTo (0.25 * width, height * progress)
            ctx.restore()
    member ctx.cloud col x y width height progress =
        ctx.save ()
        ctx.globalAlpha <- ctx.globalAlpha * progress
        ctx.strokeStyle <- color "#fff"
        ctx.fillStyle <- col
        ctx.lineWidth <- 3.
        ctx.beginPath ()
        ctx.fluffyEllipse (x, y, width, height)
        ctx.fill ()
        ctx.stroke ()
        ctx.restore ()
    member ctx.label (serifText, mathText, x, y, ?progress) =
        ctx.save ()
        ctx.lineWidth <- 1.
        ctx.font <- serifFont 55
        let serifWidth = ctx.measureText(serifText).width
        ctx.font <- mathFont 55
        let mathWidth = ctx.measureText(mathText).width
        let width = serifWidth + mathWidth
        ctx.drawLongText ([[ font (serifFont 55) serifText; font (mathFont 55) mathText ]], x - 0.5 * width, y, ?progress = progress)
        ctx.restore ()
