module App

open Animation
open Canvas
open System
open Fable.Core
open Browser.Dom
open Browser.Types

let cnv = document.getElementById "animation" :?> HTMLCanvasElement
let ctx = cnv.getContext_2d ()

module Sun =
    type Sun =
        { Radius : float
          RayLength : float
          RayRotation : float }
    
    let draw sun (ctx : CanvasRenderingContext2D) =
        let yellow = "rgba(250, 200, 20, 1)"
        ctx.save ()

        ctx.fillStyle <- U3.Case1 yellow
        ctx.beginPath ()
        ctx.ellipse (0., 0., sun.Radius)
        ctx.fill ()

        ctx.strokeStyle <- U3.Case1 yellow
        ctx.lineWidth <- 2.
        for angle in 0. .. (0.1 * Math.PI) .. 2. * Math.PI do
            ctx.save ()
            ctx.rotate (sun.RayRotation + angle)
            ctx.moveTo (0., sun.Radius + 0.1 * sun.RayLength)
            ctx.lineTo (0., sun.Radius + sun.RayLength)
            ctx.stroke()
            ctx.restore ()

        ctx.restore ()

module Cloud =
    type Cloud =
        { Size : float }

    let draw cloud (ctx : CanvasRenderingContext2D) =
        ctx.save ()

        ctx.scale (cloud.Size, cloud.Size)
        ctx.beginPath ()
        ctx.moveTo (0., 0.)
        ctx.ellipse (0., -10., 20.)
        ctx.ellipse (40., -15., 40., 30.)
        ctx.ellipse (10., 5., 10.)

        ctx.fillStyle <- rgba (200, 200, 200, 0.9)
        ctx.fill ()

        ctx.restore ()

type AnimationVars = SunRadius | SunRayLength | SunRayRotation | CloudX

let sunshine = scene {
    run (animation {
        timeline' (Alternate, Infinite) {
            0 => vars {
                SunRadius => 100
                SunRayLength => 50
            }
            600 => vars {
                SunRadius => 102
            }
            1000 => vars {
                SunRayLength => (100, EaseOutBack)
            }
        }
        timeline' (Normal, Infinite) {
            0 => vars {
                SunRayRotation => 0
                // CloudX => -200
            }
            3000 => vars {
                SunRayRotation => 0.5 * Math.PI
                //CloudX => (cnv.width + 100., EaseOutSine)
            }
        }
        2000 => timeline' (Alternate, Repeat 3) {
            0 => vars {
                CloudX => -200
            }
            2000 => vars {
                CloudX => (cnv.width + 100., EaseOutSine)
            }
        }
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.fillStyle <- U3.Case1 "rgba(10, 100, 200, 1)"
        ctx.clearRect (0., 0., cnv.width, cnv.height)

        ctx.save ()
        ctx.translate (250., 200.)
        ctx |> Sun.draw {
            Sun.Radius = tl.[SunRadius]
            Sun.RayLength = tl.[SunRayLength]
            Sun.RayRotation = tl.[SunRayRotation]
        }
        ctx.restore ()

        ctx.save ()
        ctx.translate (tl.[CloudX], 350.)
        ctx |> Cloud.draw { Cloud.Size = 2. }
        ctx.restore ()
    )
}

let square = scene {
    enter (timeline' (Alternate, Repeat 3) {
        100 => vars {
            "rectSize" => 0
        }
        1000 => vars {
            "rectSize" => 10
            "rectOffset" => 0.
        }
        2000 => vars {
            "rectSize" => (100, EaseInOutBack)
            "rectOffset" => 20.
        }
    })
    // TODO: run should not be needed here
    run (timeline { 0 => vars { "rectSize" => 100; "rectOffset" => 20. } })
    leave (timeline {
        0 => vars {
            // TODO: get the starting value from the preceding timeline
            "rectSize" => 100
        }
        1000 => vars {
            "rectSize" => 0
        }
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.clearRect (0., 0., cnv.width, cnv.height)
        ctx.beginPath ()
        let offset = tl.["rectOffset"]
        let size = tl.["rectSize"]
        ctx.rect (offset, offset, size, size)
        ctx.fillStyle <- rgb (255, 20, 100)
        ctx.fill ()
    )
}

let grid size style (ctx : CanvasRenderingContext2D) =
    ctx.save ()
    ctx.lineWidth <- 1.
    ctx.strokeStyle <- style
    ctx.fillStyle <- style
    ctx.font <- sprintf "%ipx sans-serif" (int size / 4)
    ctx.textBaseline <- "top"
    for i in 0. .. size .. ctx.width do
        ctx.beginPath ()
        ctx.moveTo (i, 0.)
        ctx.lineTo (i, ctx.height)
        ctx.stroke ()
        ctx.fillText (string i, i + (size / 10.), size / 10.)
    ctx.textBaseline <- "alphabetic"
    for i in 0. .. size .. ctx.height do
        ctx.beginPath ()
        ctx.moveTo (0., i)
        ctx.lineTo (ctx.width, i)
        ctx.stroke ()
        ctx.fillText (string i, size / 10., i - (size / 10.))
    ctx.restore ()

let arrow = scene {
    enter (timeline {
        0 => vars {
            "gridOpacity" => 0
        }
        500 => vars {
            "gridOpacity" => 1
        }
    })

    run (timeline' (Normal, Infinite) {
        0 => vars {
            "arrowDraw" => 0
            // TODO: don't require values to be set in consequitive timelines
            "gridOpacity" => 1
        }
        1000 => vars {
            "arrowDraw" => (1, EaseOutSine)
        }
        2000 => vars {
            "arrowErase" => 0
        }
        3000 => vars {
            "arrowErase" => (1, EaseInSine)
        }
        4000 => []
    } |> Timeline.delay 1000)

    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.background (rgb (0, 0, 0))
        ctx.save ()
        ctx.translate (10., 10.)

        ctx |> grid 50. (rgba (150, 150, 150, tl.["gridOpacity"]))

        ctx.lineWidth <- 3.
        ctx.strokeStyle <- rgb (255, 100, 50)
        ctx.fillStyle <- rgb (255, 0, 0)
        ctx.arrow (100., 150., 400., 150., 50., tl.["arrowDraw"], tl.["arrowErase"])

        ctx.beginPath ()
        ctx.curve (100., 200., 400., 200., 50., tl.["arrowDraw"], tl.["arrowErase"])
        ctx.stroke ()

        ctx.restore()
    )
}

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

open CodeColors

let serifFont size = sprintf "%ipx 'CMU Serif', serif" size
let mathFont size = sprintf "%ipx 'CMU Serif', math" size
let codeFont size = sprintf "%ipx Consolas, monospace" size

let helloWorld = scene {
    run (timeline' (Alternate, Infinite) {
        1000 => vars {
            "progress" => 0
        }
        2000 => vars {
            "progress" => 1
        }
        3000 => []
    })
    render (fun (ctx : CanvasRenderingContext2D) tl t ->
        ctx.font <- serifFont 200
        ctx.setStyle (color "#fff")
        ctx.textBaseline <- "middle"

        let x = (ctx.width - ctx.measureText("Hello, world!").width) / 2.
        ctx.drawText ("Hello, world!", x, ctx.height / 2., tl.["progress"], 0.5)
    )
}

let haskell = scene {
    run (timeline' (Alternate, Infinite) {
        1000 => vars {
            "progress" => 0
        }
        2000 => vars {
            "progress" => 1
        }
        3000 => []
    })
    render (fun (ctx : CanvasRenderingContext2D) tl t ->
        ctx.background (rgb (0, 0, 0))
        ctx.save ()

        let text = 
            [ [ (funcDecl, "id"); (operator, " :: "); (var, "a"); (operator, " -> "); (var, "a") ]
              [ (text, "id "); (text, "x"); (text, " = "); (text, "x") ] ]
        
        ctx.font <- codeFont 200
        ctx.setStyle (color "#fff")
        ctx.textBaseline <- "top"
        ctx.drawLongText (text, 100., (ctx.height - float text.Length * ctx.currentLineHeight * 1.2) / 2., tl.["progress"])

        ctx.restore ()
    )
}

let inline fadeIn duration easing property = timeline {
    0 => vars { property => 0. }
    (float duration) => vars { property => (1., easing) }
}

let inline fadeOut duration easing property = timeline {
    0 => vars { property => 1. }
    (float duration) => vars { property => (0., easing) }
}

type TextAlign = 
    | LeftAbove | CenterAbove | RightAbove
    | Left                    | Right
    | LeftUnder | CenterUnder | RightUnder

let node name x y align opacity (ctx : CanvasRenderingContext2D) =
    ctx.save ()
    ctx.strokeStyle <- color "#fff"
    ctx.fillStyle <- rgba (255, 255, 255, opacity)
    ctx.beginPath ()
    ctx.ellipse (x, y, 25., endAngle = opacity * 2. * Math.PI)
    ctx.stroke ()
    ctx.beginPath ()
    ctx.ellipse (x, y, 25.)
    ctx.fill ()

    ctx.setStyle (color "#fff")
    ctx.font <- mathFont 74
    let (textX, textY, baseline) =
        let width = ctx.measureText(name).width
        match align with
        | LeftAbove -> (x - width, y - 35., "bottom")
        | CenterAbove -> (x - width / 2., y - 35., "bottom")
        | RightAbove -> (x, y - 35., "bottom")
        | Left -> (x - width - 35., y, "middle")
        | Right -> (x + 35., y, "middle")
        | LeftUnder -> (x - width, y + 35., "top")
        | CenterUnder -> (x - width / 2., y + 35., "top")
        | RightUnder -> (x, y + 35., "top")
    ctx.textBaseline <- baseline
    ctx.drawText (name, textX, textY, opacity)
    ctx.restore ()

let category = scene {
    run (animation {
        fadeIn 1000 Linear "ObjectVoid"
        fadeIn 1000 Linear "ObjectUnit"
        fadeIn 1000 Linear "ObjectBool"
        1000 => fadeIn 1000 EaseOutQuad "ArrowVoidUnit"
        1000 => fadeIn 1000 EaseOutQuad "ArrowVoidBool"
        2000 => fadeIn 1000 EaseOutQuad "ArrowUnitBool"
        3000 => fadeIn 1000 EaseOutQuad "ArrowBoolUnit"
        4000 => fadeIn 1000 EaseOutQuad "ArrowIdentity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl t ->
        ctx.background (color "#000")
        ctx.save ()

        // ctx |> grid 100. (color "#eee")

        ctx |> node "Unit" 250. 700. LeftUnder tl.["ObjectUnit"]
        ctx |> node "Bool" 800. 700. RightUnder tl.["ObjectBool"]
        ctx |> node "Void" 525. 300. CenterAbove tl.["ObjectVoid"]

        ctx.strokeStyle <- color "#fff"
        ctx.lineWidth <- 5.

        ctx.arrow (490., 300., 250., 665., 100., tl.["ArrowVoidUnit"])
        ctx.arrow (560., 300., 800., 665., -100., tl.["ArrowVoidBool"])
        ctx.arrow (285., 690., 765., 690., -50., tl.["ArrowUnitBool"])
        ctx.arrow (285., 690., 765., 690., -100., tl.["ArrowUnitBool"])
        ctx.arrow (765., 710., 285., 710., -50., tl.["ArrowBoolUnit"])

        ctx.arcArrow (835., 695., 25., 0.6 * Math.PI, (0.6 - 1.25 * tl.["ArrowIdentity"]) * Math.PI, true)

        ctx.restore ()
    )
}

let inline fromTo from to' duration easing property = timeline {
    0 => vars { property => (float from) }
    (float duration) => vars { property => (float to', easing) }
}

let inline flyInBottom toY duration property = 
    fromTo ctx.height toY duration EaseOutQuad property

let drawLanguageIndicator lang progress (ctx : CanvasRenderingContext2D) =
    ctx.save ()
    ctx.setStyle (color "#fffc")
    ctx.lineWidth <- 1.
    ctx.font <- codeFont 60
    ctx.textBaseline <- "alphabetic"
    ctx.drawText (lang, 50., ctx.height - 50., progress)
    ctx.restore ()

let types = scene {
    run (animation {
        0 => flyInBottom 200. 1000 "bool"
        0 => flyInBottom 350. 1000 "int"
        0 => flyInBottom 600. 1000 "string"
        0 => flyInBottom 500. 1000 "char"
        0 => flyInBottom 250. 1000 "float"
        0 => flyInBottom 800. 1000 "double"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.fillStyle <- typeDecl
        ctx.font <- codeFont 100
        ctx.textBaseline <- "top"

        ctx.fillText ("bool", 100., tl.["bool"])
        ctx.fillText ("int", 350., tl.["int"])
        ctx.fillText ("string", 200., tl.["string"])
        ctx.fillText ("char", 1350., tl.["char"])
        ctx.fillText ("float", 1400., tl.["float"])
        ctx.fillText ("double", 1300., tl.["double"])
    )
}

let boolSet = scene {
    run (animation {
        0 => fromTo 0 (2. * Math.PI) 1000 EaseInQuad "circle"
        500 => fadeIn 1000 Linear "values"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl t ->
        ctx.fillStyle <- typeDecl
        ctx.font <- codeFont 100
        ctx.textBaseline <- "top"

        ctx.fillText ("Bool", 100., 200.)

        ctx.strokeStyle <- color "#fff"
        ctx.lineWidth <- 5.

        ctx.beginPath ()
        ctx.ellipse (400., 600., 300., endAngle = tl.["circle"], rotation = -0.5*Math.PI)
        ctx.stroke ()

        ctx.lineWidth <- 1.
        ctx.setStyle keyword
        ctx.drawText ("true", 210., 450., tl.["values"])
        ctx.drawText ("false", 350., 650., tl.["values"])
    )
}

let intSet = scene {
    run (animation {
        0 => fromTo 0 (2. * Math.PI) 1000 EaseInQuad "circle"
        500 => fadeIn 1000 Linear "values"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl t ->
        ctx.fillStyle <- typeDecl
        ctx.font <- codeFont 100
        ctx.textBaseline <- "top"

        ctx.fillText ("Int", 100., 200.)

        ctx.strokeStyle <- color "#fff"
        ctx.lineWidth <- 5.

        ctx.beginPath ()
        ctx.ellipse (400., 600., 300., endAngle = tl.["circle"], rotation = -0.5*Math.PI)
        ctx.stroke ()

        ctx.lineWidth <- 1.
        ctx.setStyle numberLit
        ctx.drawText ("1", 210., 450., tl.["values"])
        ctx.drawText ("-48", 140., 580., tl.["values"])
        ctx.drawText ("3", 360., 510., tl.["values"])
        ctx.drawText ("7", 550., 530., tl.["values"])
        ctx.drawText ("-9", 400., 350., tl.["values"])
        ctx.drawText ("512", 390., 630., tl.["values"])
        ctx.drawText ("-348", 300., 750., tl.["values"])
    )
}

let intRange = scene {
    run (animation {
        fadeIn 1000 Linear "range"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl t ->
        ctx.fillStyle <- typeDecl
        ctx.font <- codeFont 100
        ctx.textBaseline <- "top"

        ctx.fillText ("Int", 100., 200.)

        ctx.drawLongText ([
            [ (operator, "[ "); (numberLit, "−2147483648") ] 
            [ (operator, " .. "); (numberLit, "2147483647"); (operator, " ]") ]
        ], 110., 350., tl.["range"])
    )
}

let stringSet = scene {
    run (animation {
        1750 => fromTo 0. -2500. 6000 Linear "shiftUp"
        for i in 0 .. 32 do
            let var = (sprintf "str%i" i)
            (float i * 250.) => timeline {
                0 => vars { var => 0 }
                500 => vars { var => 1 }
                4000 => vars { var => 1 }
                4500 => vars { var => 0 }
            }
    })
    render (fun (ctx : CanvasRenderingContext2D) tl t ->
        ctx.translate (0., tl.["shiftUp"])

        ctx.fillStyle <- typeDecl
        ctx.font <- codeFont 100
        ctx.textBaseline <- "top"

        ctx.fillText ("String", 100., 200.)
        
        ctx.setStyle stringLit

        for i in 0 .. 32 do
            let str = sprintf "\"%s\"" (String.replicate i "a")
            ctx.drawText (str, 110., 310. + float i * 100., tl.[(sprintf "str%i" i)])
    )
}

let unitSet = scene {
    run (animation {
        0 => fromTo 0 (2. * Math.PI) 1000 EaseInQuad "circle"
        500 => fadeIn 1000 Linear "values"
        2000 => fadeIn 1000 Linear "void"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl t ->
        ctx.fillStyle <- typeDecl
        ctx.font <- codeFont 100
        ctx.textBaseline <- "top"

        ctx.fillText ("Unit", 100., 200.)
        
        ctx.drawLongText ([ [ (text, "("); (keyword, "void"); (text, ")") ] ], 500., 200., tl.["void"])

        ctx.strokeStyle <- color "#fff"
        ctx.lineWidth <- 5.

        ctx.beginPath ()
        ctx.ellipse (400., 600., 300., endAngle = tl.["circle"], rotation = -0.5*Math.PI)
        ctx.stroke ()

        ctx.lineWidth <- 1.
        ctx.setStyle operator
        ctx.textBaseline <- "middle"
        let x = 400. - ctx.measureText("()").width / 2.
        ctx.drawText ("()", x, 600., tl.["values"])
    )
}

let voidIsUnit = scene {
    run (animation {
        0 => fadeIn 1000 Linear "code"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl t ->
        ctx.font <- codeFont 70
        ctx.textBaseline <- "top"
        ctx.drawLongText([
            [ (keyword, "void "); (funcDecl, "Ignore"); (operator, "<"); (typeDecl, "T"); (operator, ">(")
              (typeDecl, "T "); (var, "value"); (operator, ")") ]
            [ (operator, "{") ]
            [ (keyword, "  "); (control, "return"); (operator, ";") ]
            [ (operator, "}") ]
        ], 100., 100., tl.["code"])

        ctx.drawLongText([
            [ (keyword, "public sealed class "); (typeDecl, "Unit") ] 
            [ (operator, "{") ]
            [ (keyword, "  public static "); (typeDecl, "Unit "); (var, "Instance"); (operator, " = "); 
              (keyword, "new "); (typeDecl, "Unit"); (operator, "();") ]
            [ (keyword, "  private "); (funcDecl, "Unit"); (operator, "() { }") ]
            [ (operator, "}") ]
        ], 100., 450., tl.["code"])

        ctx.drawLongText([
            [ (typeDecl, "Unit "); (funcDecl, "Ignore"); (operator, "<"); (typeDecl, "T"); (operator, ">(")
              (typeDecl, "T "); (var, "value"); (operator, ")") ]
            [ (operator, "{") ]
            [ (keyword, "  "); (control, "return "); (var, "Unit"); (operator, "."); (var, "Instance"); (operator, ";") ]
            [ (operator, "}") ]
        ], 100., 800., tl.["code"])

        ctx |> drawLanguageIndicator "C#" tl.["code"]
    )
}

let voidSet = scene {
    run (animation {
        0 => fromTo 0 (2. * Math.PI) 1000 EaseInQuad "circle"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl t ->
        ctx.fillStyle <- typeDecl
        ctx.font <- codeFont 100
        ctx.textBaseline <- "top"

        ctx.fillText ("Void", 100., 200.)
        
        ctx.strokeStyle <- color "#fff"
        ctx.lineWidth <- 5.

        ctx.beginPath ()
        ctx.ellipse (400., 600., 300., endAngle = tl.["circle"], rotation = -0.5*Math.PI)
        ctx.stroke ()
    )
}

let voidOO = scene {
    run (animation {
        0 => fadeIn 1000 Linear "code"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.font <- codeFont 70
        ctx.textBaseline <- "top"
        ctx.drawLongText([
            [ (keyword, "public sealed class "); (typeDecl, "Void") ]
            [ (operator, "{") ]
            [ (keyword, "  private "); (funcDecl, "Void"); (operator, "() { }") ]
            [ (operator, "}") ]
        ], 100., 100., tl.["code"])
        
        ctx |> drawLanguageIndicator "C#" tl.["code"]
    )
}

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

let typeSetsFunctions = scene {
    run (animation {
        timeline' (Alternate, Infinite) {
            0 => vars { "seed" => 1.1 }
            60000 => vars { "seed" => 2.4 }
        }
        0 => fadeIn 1000 Linear "init"

        2000 => fadeIn 1000 EaseOutCubic "intbool"
        4000 => fadeIn 1000 EaseInCubic "intboolout"

        6000 => fadeIn 1000 EaseOutCubic "boolint"
        8000 => fadeIn 1000 EaseInCubic "boolintout"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.setStyle typeDecl
        ctx.font <- codeFont 70

        ctx.globalAlpha <- tl.["init"]

        ctx.drawText ("Int", 100., 300., tl.["init"])
        ctx |> typeCloud 200. 500. 100. 150. 0. tl

        ctx.drawText ("Bool", 500., 230., tl.["init"])
        ctx |> typeCloud 550. 450. 90. 160. (15./7.) tl

        ctx.setStyle funcDecl
        ctx.textBaseline <- "top"

        ctx.drawText ("isOdd", 100., 100., tl.["intbool"] - tl.["intboolout"])
        ctx |> func 180. 450. 530. 400. -70. tl.["intbool"] tl.["intboolout"]
        ctx |> func 200. 480. 580. 405. 60. tl.["intbool"] tl.["intboolout"]
        ctx |> func 150. 530. 580. 405. 80. tl.["intbool"] tl.["intboolout"]

        ctx.drawText ("toInt", 100., 100., tl.["boolint"] - tl.["boolintout"])
        ctx |> func 550. 400. 180. 470. 50. tl.["boolint"] tl.["boolintout"]
        ctx |> func 550. 500. 210. 540. -10. tl.["boolint"] tl.["boolintout"]
    )
}

let haskellToInt = scene {
    run (animation {
        0 => fadeIn 1000 Linear "init"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.font <- codeFont 70
        ctx.drawLongText([
            [ (funcDecl, "toInt "); (operator, ":: "); (typeDecl, "Bool"); (operator, " -> "); (typeDecl, "Int") ]
            [ (text, "toInt False = "); (numberLit, "0") ]
            [ (text, "toInt True = "); (numberLit, "1") ]
        ], 100., 450., tl.["init"])
        ctx |> drawLanguageIndicator "Haskell" tl.["init"]
    )
}

let setDefinesFunc = scene {
    run (animation {
        timeline' (Alternate, Infinite) {
            0 => vars { "seed" => 1.4 }
            60000 => vars { "seed" => 2.9 }
        }
        0 => fadeIn 1000 Linear "init"

        1000 => fadeIn 1000 EaseOutCubic "ignore"
        3000 => fadeIn 1000 EaseInCubic "ignoreout"

        5000 => fadeIn 1000 EaseOutCubic "absurd"
        7000 => fadeIn 1000 EaseInCubic "absurdout"

        9000 => fadeIn 1000 EaseOutCubic "id"
        11000 => fadeIn 1000 EaseInCubic "idout"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.setStyle typeDecl
        ctx.font <- codeFont 70

        ctx.globalAlpha <- tl.["init"]

        ctx.drawText ("Void", 400., 250., tl.["init"])
        ctx |> typeCloud 500. 400. 100. 100. 0. tl

        ctx.textBaseline <- "top"

        ctx.drawText ("Bool", 200., 900., tl.["init"])
        ctx |> typeCloud 300. 700. 90. 140. (15./7.) tl

        ctx.drawText ("Unit", 680., 860., tl.["init"])
        ctx |> typeCloud 700. 700. 100. 110. (29./12.) tl

        ctx |> func 500. 400. 700. 700. -100. tl.["ignore"] tl.["ignoreout"]
        ctx |> func 280. 720. 700. 700. 40. tl.["ignore"] tl.["ignoreout"]

        ctx |> func 500. 400. 700. 700. -70. tl.["absurd"] tl.["absurdout"]
        ctx |> func 500. 400. 280. 650. 100. tl.["absurd"] tl.["absurdout"]
        ctx |> func 500. 400. 310. 730. -60. tl.["absurd"] tl.["absurdout"]

        ctx.save ()
        ctx |> setFuncStyle
        ctx.arcArrow (560., 400., 70., -0.9 * Math.PI + 1.8 * Math.PI * tl.["idout"], -0.9 * Math.PI + 1.8 * Math.PI * tl.["id"])
        ctx.arcArrow (240., 700., 70., 0.1 * Math.PI + 1.8 * Math.PI * tl.["idout"], 0.1 * Math.PI + 1.8 * Math.PI * tl.["id"])
        ctx.arcArrow (770., 700., 70., -0.9 * Math.PI + 1.8 * Math.PI * tl.["idout"], -0.9 * Math.PI + 1.8 * Math.PI * tl.["id"])
        ctx.restore ()
    )
}

let sideEffect = scene {
    run (animation {
        0 => fadeIn 1000 Linear "init"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.font <- codeFont 70
        ctx.textBaseline <- "top"
        ctx.drawLongText([
            [ (keyword, "public "); (typeDecl, "double "); (funcDecl, "Square"); (text, "(")
              (typeDecl, "double "); (text, "x) {") ]
            [ (text, "  System.out."); (funcDecl, "println"); (text, "("); (stringLit, "\"Squaring \""); (text, " + x);") ]
            [ (control, "  return "); (text, "x * x;") ]
            [ (text, "}") ]
        ], 100., 100., tl.["init"])
        ctx |> drawLanguageIndicator "Java" tl.["init"]
    )
}

let scenes =
    [ // 1: Types and functions
      types; boolSet; intSet; intRange; stringSet; unitSet; voidIsUnit; voidSet; voidOO
      typeSetsFunctions; haskellToInt; setDefinesFunc
      // 2: Why types?
      sideEffect ]

let intro = scene {
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

let ind =
    match Int32.TryParse(window.location.hash.TrimStart('#')) with
    | true, i when i >= 0 -> i
    | true, i -> scenes.Length + i
    | false, _ -> 0

window.onhashchange <- fun _ -> window.location.reload (true)

scenes.[ind]
|> Scene.withRender (fun (ctx : CanvasRenderingContext2D) tl t render -> 
    ctx.background (color "#000")
    ctx.save ()
    // ctx |> grid 100. (color "#eee")
    render ctx tl t
    ctx.restore ()
)
|> Scene.run ctx