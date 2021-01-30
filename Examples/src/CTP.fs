module CTP

open System
open Browser.Types
open Animation.Animation
open Canvas

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
    ctx.font <- serifFont 74
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

let types = scene "types" {
    enter (animation {
        0 => fadeIn 1000 EaseOutQuad "enter"
    })
    leave (animation {
        0 => fadeIn 1000 EaseInQuad "leave"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.fillStyle <- typeDecl
        ctx.font <- codeFont 100
        ctx.textBaseline <- "top"

        let flyInOut y =
            ctx.height - (ctx.height - y) * tl.["enter"] - (y + 120.) * tl.["leave"]

        ctx.fillText ("bool", 100., flyInOut 200.)
        ctx.fillText ("int", 350., flyInOut 350.)
        ctx.fillText ("string", 200., flyInOut 600.)
        ctx.fillText ("char", 1350., flyInOut 500.)
        ctx.fillText ("float", 1400., flyInOut 250.)
        ctx.fillText ("double", 1300., flyInOut 800.)
    )
}

let boolSet = scene "boolSet" {
    enter (animation {
        0 => fadeIn 750 Linear "title"
        500 => fromTo 0 (2. * Math.PI) 1000 EaseInQuad "circle"
        1000 => fadeIn 750 Linear "values"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]
        ctx.setStyle typeDecl
        ctx.font <- codeFont 100
        ctx.textBaseline <- "top"

        ctx.drawText ("Bool", 100., 200., tl.["title"])

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

let intSet = scene "intSet" {
    enter (animation {
        0 => fadeIn 750 Linear "title"
        500 => fromTo 0 (2. * Math.PI) 1000 EaseInQuad "circle"
        1000 => fadeIn 750 Linear "values"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.setStyle typeDecl
        ctx.font <- codeFont 100
        ctx.textBaseline <- "top"

        ctx.drawText ("Int", 100., 200., tl.["title"])

        ctx.globalAlpha <- tl.["opacity"]
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

let intRange = scene "intRange" {
    enter (animation {
        0 => fadeIn 1000 Linear "range"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]
        ctx.setStyle typeDecl
        ctx.font <- codeFont 100
        ctx.textBaseline <- "top"

        ctx.fillText ("Int", 100., 200.)

        ctx.drawLongText ([
            [ (operator, "[ "); (numberLit, "−2147483648") ] 
            [ (operator, " .. "); (numberLit, "2147483647"); (operator, " ]") ]
        ], 110., 350., tl.["range"])
    )
}

let stringSet = scene "stringSet" {
    enter (animation {
        0 => fadeIn 750 Linear "title"
    })
    run (animation {
        1750 => fromTo 0. -2500. 6000 Linear "shiftUp"
        for i in 0 .. 32 do
            let var = (sprintf "str%i" i)
            (i * 250) => timeline {
                0 => vars { var => 0 }
                500 => vars { var => 1 }
            }
            9000 => timeline {
                0 => vars { var => 1 }
                500 => vars { var => 0 }
            }
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.translate (0., tl.["shiftUp"])
        ctx.setStyle typeDecl
        ctx.font <- codeFont 100
        ctx.textBaseline <- "top"

        ctx.drawText ("String", 100., 200., tl.["title"])
        
        ctx.setStyle stringLit

        for i in 0 .. 32 do
            let str = sprintf "\"%s\"" (String.replicate i "a")
            ctx.drawText (str, 110., 310. + float i * 100., tl.[(sprintf "str%i" i)])
    )
}

let unitSet = scene "unitSet" {
    enter (animation {
        0 => fadeIn 750 Linear "title"
        500 => fromTo 0 (2. * Math.PI) 1000 EaseInQuad "circle"
        1000 => fadeIn 750 Linear "values"
    })
    run (animation {
        0 => fadeIn 1000 Linear "void"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]
        ctx.setStyle typeDecl
        ctx.font <- codeFont 100
        ctx.textBaseline <- "top"

        ctx.drawText ("Unit", 100., 200., tl.["title"])
        
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

let voidIsUnit = scene "voidIsUnit" {
    enter (animation {
        0 => fadeIn 750 Linear "code"
        0 => fadeIn 750 Linear "code_void"
    })
    run (animation {
        0 => fadeIn 750 Linear "class_unit"
        1000 => fadeOut 750 Linear "code_void"
        1250 => fadeIn 750 Linear "code_unit"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]
        ctx.font <- codeFont 70
        ctx.textBaseline <- "top"

        ctx.drawLongText([
            [ (keyword, "public sealed class "); (typeDecl, "Unit") ] 
            [ (operator, "{") ]
            [ (keyword, "  public static "); (typeDecl, "Unit "); (var, "Instance"); (operator, " = "); 
              (keyword, "new "); (typeDecl, "Unit"); (operator, "();") ]
            [ (keyword, "  private "); (funcDecl, "Unit"); (operator, "() { }") ]
            [ (operator, "}") ]
        ], 100., 100., tl.["class_unit"])

        ctx.drawLongText([
            [ (keyword, "     "); (funcDecl, "Ignore"); (operator, "<"); (typeDecl, "T"); (operator, ">(")
              (typeDecl, "T "); (var, "value"); (operator, ")") ]
            [ (operator, "{") ]
            [ (keyword, "  "); (control, "return"); ]
            [ (operator, "}") ]
        ], 100., 520., tl.["code"])

        ctx.drawLongText([
            [ (keyword, "void") ]
            [ ]
            [ (text, "        "); (operator, ";") ]
        ], 100., 520., tl.["code_void"])

        ctx.drawLongText([
            [ (typeDecl, "Unit") ]
            [ ]
            [ (text, "         "); (var, "Unit"); (operator, "."); (var, "Instance"); (operator, ";") ]
        ], 100., 520., tl.["code_unit"])

        ctx |> drawLanguageIndicator "C#" tl.["code"]
    )
}

let voidSet = scene "voidSet" {
    enter (animation {
        0 => fadeIn 750 Linear "title"
        500 => fromTo 0 (2. * Math.PI) 1000 EaseInQuad "circle"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.fillStyle <- typeDecl
        ctx.font <- codeFont 100
        ctx.textBaseline <- "top"

        ctx.drawText ("Void", 100., 200., tl.["title"])
        
        ctx.globalAlpha <- tl.["opacity"]
        ctx.strokeStyle <- color "#fff"
        ctx.lineWidth <- 5.

        ctx.beginPath ()
        ctx.ellipse (400., 600., 300., endAngle = tl.["circle"], rotation = -0.5*Math.PI)
        ctx.stroke ()
    )
}

let voidOO = scene "voidOO" {
    enter (animation {
        0 => fadeIn 1000 Linear "code"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]
        ctx.fillStyle <- typeDecl
        ctx.font <- codeFont 100
        ctx.textBaseline <- "top"

        ctx.fillText ("Void", 100., 200.)

        ctx.font <- codeFont 70
        ctx.textBaseline <- "top"
        ctx.drawLongText([
            [ (keyword, "public sealed class "); (typeDecl, "Void") ]
            [ (operator, "{") ]
            [ (keyword, "  private "); (funcDecl, "Void"); (operator, "() { }") ]
            [ (operator, "}") ]
        ], 100., 350., tl.["code"])
        
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

let typeSetsFunctions = scene "typeSetsFunctions" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
    })
    run (animation {
        // TODO: create some sort of background animation thing for this
        timeline {
            0 => vars { "seed" => 1.1 }
            23750 => vars { "seed" => 1.7 }
        }

        500 => fadeIn 750 EaseOutCubic "intbool"
        11500 => fadeIn 750 EaseInCubic "intboolout"

        12750 => fadeIn 750 EaseOutCubic "boolint"
        23750 => fadeIn 750 EaseInCubic "boolintout"
    })
    leave (animation {
        0 => fadeOut 500 Linear "init"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.setStyle typeDecl
        ctx.font <- codeFont 70


        ctx.drawText ("Int", 100., 300., tl.["init"])
        ctx.drawText ("Bool", 500., 230., tl.["init"])
        ctx.globalAlpha <- tl.["init"]
        ctx |> typeCloud 200. 500. 100. 150. 0. tl
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

let haskellToInt = scene "haskellToInt" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]
        ctx.font <- codeFont 70
        ctx.drawLongText([
            [ (funcDecl, "toInt "); (operator, ":: "); (typeDecl, "Bool"); (operator, " -> "); (typeDecl, "Int") ]
            [ (text, "toInt False = "); (numberLit, "0") ]
            [ (text, "toInt True = "); (numberLit, "1") ]
        ], 100., 450., tl.["init"])
        ctx |> drawLanguageIndicator "Haskell" tl.["init"]
    )
}

let setDefinesFunc = scene "setDefinesFunc" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
    })
    run (animation {
        timeline {
            0 => vars { "seed" => 1.4 }
            109000 => vars { "seed" => 3.1 }
        }

        0 => fadeIn 750 EaseOutCubic "ignore"
        28250 => fadeIn 750 EaseInCubic "ignoreout"

        30000 => fadeIn 750 EaseOutCubic "absurd"
        72000 => fadeIn 750 EaseInCubic "absurdout"

        73500 => fadeIn 750 EaseOutCubic "id"
        108250 => fadeIn 750 EaseInCubic "idout"
    })
    leave (animation {
        0 => fadeOut 500 Linear "init"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.setStyle typeDecl
        ctx.font <- codeFont 70


        ctx.drawText ("Void", 400., 250., tl.["init"])
        ctx.textBaseline <- "top"
        ctx.drawText ("Bool", 200., 900., tl.["init"])
        ctx.drawText ("Unit", 680., 860., tl.["init"])
        
        ctx.globalAlpha <- tl.["init"]
        ctx |> typeCloud 500. 400. 100. 100. 0. tl
        ctx |> typeCloud 300. 700. 90. 140. (15./7.) tl
        ctx |> typeCloud 700. 700. 100. 110. (29./12.) tl

        ctx |> func 500. 400. 700. 700. -100. tl.["ignore"] tl.["ignoreout"]
        ctx |> func 280. 720. 700. 700. 40. tl.["ignore"] tl.["ignoreout"]

        ctx |> func 500. 400. 700. 700. -70. tl.["absurd"] tl.["absurdout"]
        ctx |> func 500. 400. 280. 650. 100. tl.["absurd"] tl.["absurdout"]
        ctx |> func 500. 400. 310. 730. -60. tl.["absurd"] tl.["absurdout"]

        ctx.save ()
        ctx |> setFuncStyle
        ctx.arcArrow (560., 400., 70., -0.9 * Math.PI + 1.8 * Math.PI * tl.["idout"], -0.9 * Math.PI + 1.8 * Math.PI * tl.["id"])

        ctx.arcArrow (230., 700., 90., 0.1 * Math.PI + 1.8 * Math.PI * tl.["idout"], 0.1 * Math.PI + 1.8 * Math.PI * tl.["id"])
        ctx.arcArrow (240., 700., 70., 0.1 * Math.PI + 1.8 * Math.PI * tl.["idout"], 0.1 * Math.PI + 1.8 * Math.PI * tl.["id"])
        ctx.arcArrow (250., 700., 50., 0.1 * Math.PI + 1.8 * Math.PI * tl.["idout"], 0.1 * Math.PI + 1.8 * Math.PI * tl.["id"])
        ctx.arcArrow (260., 700., 30., 0.1 * Math.PI + 1.8 * Math.PI * tl.["idout"], 0.1 * Math.PI + 1.8 * Math.PI * tl.["id"])

        ctx.arcArrow (770., 700., 70., -0.9 * Math.PI + 1.8 * Math.PI * tl.["idout"], -0.9 * Math.PI + 1.8 * Math.PI * tl.["id"])
        ctx.restore ()
    )
}

let sideEffect = scene "sideEffect" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]
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

let whyTypes = scene "whyTypes" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]
        ctx.font <- serifFont 100
        ctx.setStyle (color "#fff")
        ctx.drawText ("Correctness", 200., 300., tl.["init"])
    )
}

let v01_types = 
    [ types; boolSet; intSet; intRange; stringSet; unitSet; voidIsUnit; voidSet; voidOO
      typeSetsFunctions; haskellToInt; setDefinesFunc
      sideEffect
      whyTypes ]

let composition = scene "composition" {
    run (animation {
        for i in 0 .. 5 ->
            i * 400 => fromTo -2.5 1. 500 EaseInCubic $"block_%i{i}"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        let block x y width height (colorR, colorG, colorB) =
            ctx.save ()
            ctx.lineWidth <- 5.
            ctx.fillStyle <- rgba (colorR, colorG, colorB, 0.9)
            ctx.beginPath ()
            ctx.rect (x + 0.5 * ctx.lineWidth, y + 0.5 * ctx.lineWidth, width - ctx.lineWidth, height - ctx.lineWidth)
            ctx.fill ()
            ctx.closePath ()
            ctx.restore ()
        
        let flyUp y progress =
            ctx.height - ((ctx.height - y) * progress)

        ctx.globalAlpha <- tl.["opacity"]

        ctx.save ()
        ctx.translate (400., 450.)
        ctx.rotate (0.25 * Math.PI)
        ctx.translate (-400., -450.)
        block (200. * tl.["block_0"]) 200. 200. 300. (200, 200, 70)
        block 400. (300. * tl.["block_1"]) 100. 200. (30, 180, 60)
        block 500. (flyUp 300. tl.["block_2"]) 100. 300. (160, 40, 160)
        block 400. (200. * tl.["block_3"]) 200. 100. (40, 40, 200)
        block (200. * tl.["block_4"]) 500. 300. 200. (190, 20, 60)
        block 500. (flyUp 600. tl.["block_5"]) 100. 100. (210, 160, 40)
        ctx.restore ()
    )
}

let compositionText = scene "compositionText" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]
        ctx.font <- serifFont 100
        ctx.setStyle (color "#fff")
        ctx.textBaseline <- "middle"
        ctx.drawText ("Composition", 140., ctx.height / 2., tl.["init"])
    )
}

let simpleCategory = scene "simpleCategory" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
        750 => fadeIn 750 EaseOutQuad "initialArrows"
    })
    run (animation {
        0 => fadeIn 750 EaseOutQuad "identity"
        1000 => fadeIn 750 EaseOutQuad "composition"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]

        let middle = ctx.height / 3.
        ctx |> node "" 200. middle CenterUnder tl.["init"]
        ctx |> node "" 450. middle CenterUnder tl.["init"]
        ctx |> node "" 700. middle CenterUnder tl.["init"]

        ctx.strokeStyle <- color "#fff"
        ctx.lineWidth <- 5.

        ctx.arrow (235., middle, 415., middle, 0., stagger 2 0 tl.["initialArrows"])
        ctx.arrow (485., middle, 665., middle, 0., stagger 2 1 tl.["initialArrows"])

        ctx.arcArrow (200., middle + 45., 25., -0.2 * Math.PI, (-0.2 + 1.4 * tl.["identity"]) * Math.PI)
        ctx.arcArrow (450., middle + 45., 25., -0.2 * Math.PI, (-0.2 + 1.4 * tl.["identity"]) * Math.PI)
        ctx.arcArrow (700., middle + 45., 25., -0.2 * Math.PI, (-0.2 + 1.4 * tl.["identity"]) * Math.PI)

        ctx.arrow (200., middle - 35., 700., middle - 35., -75., tl.["composition"])

        ctx.setStyle (color "#fff")
        ctx.font <- serifFont 70
        ctx.lineWidth <- 1.
        ctx.drawText ("1. Identity", 175., ctx.height * (2./3.) - 0.5 * ctx.actualLineHeight, tl.["identity"])
        ctx.drawText ("2. Composition", 175., ctx.height * (2./3.) + 0.5 * ctx.actualLineHeight, tl.["composition"])
    )
}

let jsComposition = scene "jsComposition" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
    })
    run (animation {
        0 => fadeIn 750 Linear "composition"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]
        ctx.font <- codeFont 65
        ctx.textBaseline <- "top"

        ctx.drawLongText([
            [ (keyword, "function "); (funcDecl, "toInt"); (text, "("); (var, "value"); (text, ") {") ]
            [ (control, "  return "); (var, "value"); (operator, " ? "); (numberLit, "1"); (operator, " : "); (numberLit, "0"); (text, ";") ]
            [ (text, "}") ]
        ], 100., 100., tl.["init"])

        ctx.drawLongText([
            [ (keyword, "function "); (funcDecl, "toString"); (text, "("); (var, "value"); (text, ") {") ]
            [ (control, "  return "); (var, "value"); (text, "."); (funcDecl, "toString"); (text, "();") ]
            [ (text, "}") ]
        ], 100., 100. + 4. * ctx.actualLineHeight, tl.["init"])

        ctx.drawLongText([
            [ (funcDecl, "toString"); (text, "("); (funcDecl, "toInt"); (text, "("); (keyword, "true"); (text, "));") ]
        ], 100., 100. + 8. * ctx.actualLineHeight, tl.["composition"])

        ctx |> drawLanguageIndicator "JavaScript" tl.["init"]
    )
}

let haskComposition = scene "haskComposition" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
        200 => fadeIn 650 Linear "composition"
    })
    run (animation {
        0 => fadeOut 750 Linear "composition"
        250 => fadeIn 750 Linear "pointFreeComposition"
        2000 => fadeIn 750 Linear "mathComposition"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]
        ctx.font <- codeFont 65
        ctx.textBaseline <- "top"

        ctx.drawLongText([
            [ (funcDecl, "toInt "); (operator, ":: "); (typeDecl, "Bool"); (operator, " -> "); (typeDecl, "Int") ]
            [ (text, "toInt False = "); (numberLit, "0") ]
            [ (text, "toInt True = "); (numberLit, "1") ]
        ], 100., 100., tl.["init"])

        ctx.drawLongText([
            [ (funcDecl, "toString "); (operator, ":: "); (typeDecl, "Int"); (operator, " -> "); (typeDecl, "String") ]
            [ (text, "toString value = show value") ]
        ], 100., 100. + 4. * ctx.actualLineHeight, tl.["init"])

        ctx.drawLongText([
            [ (funcDecl, "boolToString "); (operator, ":: "); (typeDecl, "Bool"); (operator, " -> "); (typeDecl, "String") ]
            [ (text, "boolToString ")]
        ], 100., 100. + 7. * ctx.actualLineHeight, tl.["init"])

        ctx.drawLongText([
            [ (text, "             value = toString (toInt value)") ]
        ], 100., 100. + 8. * ctx.actualLineHeight, tl.["composition"])

        ctx.drawLongText([
            [ (text, "             = toString . toInt") ]
        ], 100., 100. + 8. * ctx.actualLineHeight, tl.["pointFreeComposition"])

        let mathPos = 100. + 9.4 * ctx.actualLineHeight
        ctx.font <- mathFont 80
        ctx.setStyle (color "#fff")
        let compEq = "g \u2218 f"
        ctx.drawText (compEq, 895., mathPos + 30., tl.["mathComposition"])
        ctx.lineWidth <- 5.
        ctx.strokeStyle <- rgba (255, 255, 255, tl.["mathComposition"])
        ctx.strokeRect (875., mathPos, ctx.measureText(compEq).width + 60., (ctx.actualLineHeight / ctx.lineHeight) + 60.)

        ctx |> drawLanguageIndicator "Haskell" tl.["init"]
    )
}

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

let typesCategory = scene "typesCategory" {
    enter (animation {
        0 => fadeIn 750 Linear "objects"
    })
    run (animation {
        0 => fadeIn 750 EaseOutQuad "identity"
        1000 => fadeIn 750 EaseOutQuad "absurd_bool"
        2000 => fadeIn 750 EaseOutQuad "ignore_bool"
        3000 => fadeIn 750 EaseOutQuad "pick_bool"
        4000 => fadeIn 750 Linear "highlight_void_unit_path"
        5000 => fadeIn 750 EaseOutQuad "absurd_unit"
        6000 => fadeOut 500 Linear "highlight_void_unit_path"
        7000 => fadeIn 750 Linear "highlight_bool_konst"
        8000 => fadeOut 500 Linear "highlight_bool_konst"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]
        ctx |> node "Bool" 250. 700. LeftUnder tl.["objects"]
        ctx |> node "Unit" 800. 700. RightUnder tl.["objects"]
        ctx |> node "Void" 525. 300. CenterAbove tl.["objects"]

        ctx.setStyle (color "#fff")
        ctx.lineWidth <- 5.            

        ctx.highlightedArcArrow (210., 695., 25., (1. - 0.65) * Math.PI, (1. - 0.65 + 1.4 * tl.["identity"]) * Math.PI, tl.["highlight_bool_konst"])
        ctx.save ()
        ctx.font <- serifFont 35
        ctx.lineWidth <- 1.
        ctx.drawText ("4x", 150., 670., tl.["identity"])
        ctx.restore ()
        ctx.arcArrow (840., 695., 25., 0.65 * Math.PI, (0.65 - 1.4 * tl.["identity"]) * Math.PI, true)
        ctx.arcArrow (525., 345., 25., -0.2 * Math.PI, (-0.2 + 1.4 * tl.["identity"]) * Math.PI)

        ctx.highlightedArrow (490., 300., 250., 665., 100., tl.["absurd_bool"], tl.["highlight_void_unit_path"])
        ctx.highlightedArrow (285., 710., 765., 710., 50., tl.["ignore_bool"], tl.["highlight_void_unit_path"] + tl.["highlight_bool_konst"])
        ctx.arrow (765., 690., 285., 690., 25., tl.["pick_bool"])
        ctx.highlightedArrow (765., 690., 285., 690., 75., tl.["pick_bool"], tl.["highlight_bool_konst"])

        ctx.highlightedArrow (560., 300., 800., 665., -100., tl.["absurd_unit"], tl.["highlight_void_unit_path"])
    )
}

let inline interpolate a0 a1 w =
    (float a1 - float a0) * w + float a0

let compositionRules = scene "compositionRules" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        let line l = ctx.height * (1./3.) + l * ctx.actualLineHeight
        ctx.textBaseline <- "top"
        ctx.setStyle (color "#fff")

        ctx.font <- serifFont 70
        ctx.drawText ("1. Associativity", 100., interpolate (line 0.) 100. (1. - tl.["opacity"]), tl.["init"])
        ctx.globalAlpha <- tl.["opacity"]
        ctx.drawText ("2. Identity", 100., line 1., tl.["init"])
    )
}

let associativity = scene "associativity" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
        750 => fadeIn 750 EaseOutQuad "initialArrows"
        1500 => fadeIn 750 Linear "mathEnter"
    })
    run (animation {
        timeline {
            0 => vars {
                "hi_f" => 0
                "hi_g" => 0
                "hi_h" => 0
                "hi_fg" => 0
                "hi_gh" => 0
            }
        }
        for f in [ "f"; "g"; "h"] do 
            0 => fadeIn 750 Linear $"hi_{f}"
        
        1500 => fadeOut 500 Linear "hi_f"
        1500 => fadeOut 500 Linear "hi_g"
        1500 => fadeIn 750 Linear "hi_fg"

        3000 => fadeIn 750 Linear "hi_f"
        3000 => fromTo 0. 0.5 750 Linear "hi_g"
        3000 => fadeOut 500 Linear "hi_fg"
        3750 => fadeOut 500 Linear "hi_g"
        3750 => fadeOut 500 Linear "hi_h"
        3750 => fadeIn 750 Linear "hi_gh"

        6000 => fadeOut 500 Linear "hi_f"
        6000 => fadeOut 500 Linear "hi_gh"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]

        ctx.textBaseline <- "top"
        ctx.setStyle (color "#fff")
        ctx.font <- serifFont 70
        ctx.drawText ("1. Associativity", 100., 100., 1.)

        ctx.globalAlpha <- 1.

        // Objects
        let middle = ctx.height / 2.
        ctx |> node "" 200. (middle - 200.) CenterUnder tl.["init"]
        ctx |> node "" 600. (middle - 200.) CenterUnder tl.["init"]
        ctx |> node "" 600. (middle + 200.) CenterUnder tl.["init"]
        ctx |> node "" 200. (middle + 200.) CenterUnder tl.["init"]

        // Morphism styling
        ctx.strokeStyle <- color "#fff"
        ctx.lineWidth <- 5.

        let w = (255, 255, 255, 1.)
        let t = (0, 0, 0, 0.)
        let f = (13, 77, 255, 1.)
        let g = (255, 13, 17, 1.)
        let h = (49, 255, 13, 1.)
        let fg = (162, 13, 255, 1.)
        let gh = (255, 186, 13, 1.)

        let inline cp (r1, g1, b1, a1) (r2, g2, b2, a2) w = (interpolate r1 r2 w, interpolate g1 g2 w, interpolate b1 b2 w, interpolate a1 a2 w)
        let hc (r, g, b, _) = $"rgba(%i{r}, %i{g}, %i{b}, 0.06)"

        // Sides
        ctx.highlightedArrow (235., middle - 200., 565., middle - 200., 0., stagger 2 0 tl.["initialArrows"], tl.["hi_fg"], highlightColor = hc fg)
        ctx.highlightedArrow (235., middle + 200., 565., middle + 200., 0., stagger 2 1 tl.["initialArrows"], tl.["hi_gh"], highlightColor = hc gh)
        ctx.highlightedArrow (200., middle - 165., 200., middle + 165., 0., stagger 2 0 tl.["initialArrows"], tl.["hi_f"], highlightColor = hc f)
        ctx.highlightedArrow (600., middle - 165., 600., middle + 165., 0., stagger 2 1 tl.["initialArrows"], tl.["hi_h"], highlightColor = hc h)
        
        // Diagonals
        let d = sqrt (35.**2. / 2.)
        ctx.arrow (200. + d, middle - 200. + d, 600. - d, middle + 200. - d, 0., tl.["initialArrows"])
        ctx.highlightedArrow (200. + d, middle + 200. - d, 600. - d, middle - 200. + d, 0., tl.["initialArrows"], tl.["hi_g"], highlightColor = hc g)

        // Identities
        let d = sqrt (45.**2. / 2.)
        ctx.arcArrow (200. - d, middle - 200. - d, 25., 0.5 * Math.PI, (0.5 + tl.["initialArrows"] * 1.5) * Math.PI)
        ctx.arcArrow (200. - d, middle + 200. + d, 25., 0., (tl.["initialArrows"] * 1.5) * Math.PI)
        ctx.arcArrow (600. + d, middle - 200. - d, 25., Math.PI, (1. + tl.["initialArrows"] * 1.5) * Math.PI)
        ctx.arcArrow (600. + d, middle + 200. + d, 25., 1.5 * Math.PI, (1.5 + tl.["initialArrows"] * 1.5) * Math.PI)

        // Function labels
        ctx.font <- mathFont 60
        ctx.textBaseline <- "top"
        ctx.drawText ("f", 155., 400., tl.["mathEnter"])
        
        ctx.globalAlpha <- tl.["opacity"]
        
        ctx.drawText ("g", 330., 610., tl.["mathEnter"])
        ctx.drawText ("h", 620., 400., tl.["mathEnter"])


        // Math
        ctx.font <- mathFont 80
        ctx.textBaseline <- "middle"
        ctx.lineWidth <- 1.
        ctx.drawLongText([
            [ (rgba (cp t gh tl.["hi_gh"]), "(")
              (rgba (cp (cp w h tl.["hi_h"]) gh tl.["hi_gh"]), "h") 
              (rgba (cp w gh tl.["hi_gh"]), " \u2218")
              (rgba (cp t fg tl.["hi_fg"]), "(")
              (rgba (cp (cp (cp w g tl.["hi_g"]) gh tl.["hi_gh"]) fg tl.["hi_fg"]), "g")
              (rgba (cp t gh tl.["hi_gh"]), ")")
              (rgba (cp w fg tl.["hi_fg"]), "\u2218 ")
              (rgba (cp (cp w f tl.["hi_f"]) fg tl.["hi_fg"]), "f")
              (rgba (cp t fg tl.["hi_fg"]), ")") ]
        ], 750., middle, tl.["mathEnter"])
    )
}

let identity = scene "identity" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
        750 => fadeIn 750 Linear "hi_f"
    })
    run (animation {
        0 => fadeIn 750 Linear "id_f"
        0 => fadeIn 750 Linear "hi_id_f"
        1500 => fadeOut 500 Linear "hi_id_f"

        3000 => fadeIn 750 Linear "f_id"
        3000 => fadeIn 750 Linear "hi_f_id"

        5000 => fadeOut 500 Linear "hi_f_id"
        5000 => fadeOut 500 Linear "hi_f"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]

        ctx.textBaseline <- "top"
        ctx.setStyle (color "#fff")
        ctx.font <- serifFont 70
        ctx.drawText ("2. Identity", 100., 100., tl.["init"])

        // Objects
        let middle = ctx.height / 2.
        ctx |> node "" 200. (middle - 200.) CenterUnder 1.
        ctx |> node "" 600. (middle - 200.) CenterUnder 1.
        ctx |> node "" 600. (middle + 200.) CenterUnder 1.
        ctx |> node "" 200. (middle + 200.) CenterUnder 1.

        // Morphism styling
        ctx.strokeStyle <- color "#fff"
        ctx.lineWidth <- 5.

        // Sides
        ctx.arrow (235., middle - 200., 565., middle - 200., 0., 1.)
        ctx.arrow (235., middle + 200., 565., middle + 200., 0., 1.)
        ctx.highlightedArrow (200., middle - 165., 200., middle + 165., 0., 1., tl.["hi_f"])
        ctx.arrow (600., middle - 165., 600., middle + 165., 0., 1.)
        
        // Diagonals
        let d = sqrt (35.**2. / 2.)
        ctx.arrow (200. + d, middle - 200. + d, 600. - d, middle + 200. - d, 0.)
        ctx.arrow (200. + d, middle + 200. - d, 600. - d, middle - 200. + d, 0.)

        // Identities
        let d = sqrt (45.**2. / 2.)
        ctx.highlightedArcArrow (200. - d, middle - 200. - d, 25., 0.5 * Math.PI, (0.5 + 1.5) * Math.PI, tl.["hi_id_f"])
        ctx.highlightedArcArrow (200. - d, middle + 200. + d, 25., 0., 1.5 * Math.PI, tl.["hi_f_id"])
        ctx.arcArrow (600. + d, middle - 200. - d, 25., Math.PI, (1. + 1.5) * Math.PI)
        ctx.arcArrow (600. + d, middle + 200. + d, 25., 1.5 * Math.PI, (1.5 + 1.5) * Math.PI)

        // Function labels
        ctx.font <- mathFont 60
        ctx.textBaseline <- "top"
        ctx.fillText ("f", 155., 400.)

        // Math
        ctx.font <- mathFont 80
        ctx.textBaseline <- "top"
        ctx.lineWidth <- 1.
        ctx.drawText("id \u2218 f = f", 750., middle - ctx.actualLineHeight, tl.["id_f"])
        ctx.drawText("f \u2218 id = f", 750., middle, tl.["f_id"])
    )
}

let categorySet = scene "categorySet" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
    })
    run (animation {
        timeline' (Alternate, Infinite) {
            0 => vars { "seed" => 1.1 }
            3500 => vars { "seed" => 1.21 }
        }
        timeline' (Normal, Infinite) {
            for i in 0 .. 5 do
                i * 1000 => vars { $"arrow_%i{i}_in" => 0; $"arrow_%i{i}_out" => 0 }
                i * 1000 + 1000 => vars { $"arrow_%i{i}_in" => (1, EaseOutQuad) }
                i * 1000 + 1000 => vars { $"arrow_%i{i}_out" => 0 }
                i * 1000 + 1999 => vars { $"arrow_%i{i}_in" => 1; $"arrow_%i{i}_out" => (1, EaseInQuad) }
                i * 1000 + 2000 => vars { $"arrow_%i{i}_in" => 0; $"arrow_%i{i}_out" => 0 }
        }
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]

        ctx.font <- serifFont 100
        ctx.setStyle (color "#fff")
        ctx.drawText ("Set", 500., 320., tl.["init"])


        ctx.globalAlpha <- ctx.globalAlpha * tl.["init"]
        ctx.strokeStyle <- color "#def"
        ctx.fillStyle <- color "#defc"
        ctx.lineWidth <- 3.
        ctx.beginPath ()
        ctx.fluffyEllipse(400., ctx.height / 2., 150., 200., seed = tl.["seed"])
        ctx.fill ()
        ctx.stroke ()

        ctx.setStyle (color "#fff")
        ctx.beginPath ()
        ctx.ellipse (320., 406., 7.)
        ctx.fill ()
        ctx.beginPath ()
        ctx.ellipse (409., 609., 7.)
        ctx.fill ()
        ctx.beginPath ()
        ctx.ellipse (500., 500., 7.)
        ctx.fill ()
        ctx.beginPath ()
        ctx.ellipse (298., 600., 7.)
        ctx.fill ()

        ctx.lineWidth <- 2.
        ctx.arrow (320., 406., 409., 609., 20., tl.["arrow_0_in"], tl.["arrow_0_out"])
        ctx.arrow (409., 609., 500., 500., -15., tl.["arrow_1_in"], tl.["arrow_1_out"])
        ctx.arrow (298., 600., 320., 406., 25., tl.["arrow_2_in"], tl.["arrow_2_out"])
        ctx.arrow (320., 406., 500., 500., -20., tl.["arrow_3_in"], tl.["arrow_3_out"])
        ctx.arrow (500., 500., 298., 600., 10., tl.["arrow_4_in"], tl.["arrow_4_out"])
        ctx.arrow (500., 500., 320., 406., 15., tl.["arrow_5_in"], tl.["arrow_5_out"])
    )
}

let v02_categories =
    [ composition; compositionText; simpleCategory; jsComposition; haskComposition
      typesCategory; compositionRules; associativity; identity; categorySet ]

let kleisliWriter = scene "kleisliWriter" {
    enter (animation {
        0 => fadeIn 1000 Linear "init"
    })
    run (animation {
        0 => fadeIn 750 EaseInQuad "a->b"
        1000 => fadeIn 750 EaseInQuad "id"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]
        
        ctx.setStyle (color "#fff")
        ctx.lineWidth <- 3.

        let kleisli = ctx.height / 3.
        ctx.save ()
        ctx.globalAlpha <- ctx.globalAlpha * (stagger 2 0 tl.["init"])
        ctx.fillStyle <- color "#def7"
        ctx.beginPath ()
        ctx.fluffyEllipse (510., kleisli, 380., 150.)
        ctx.fill ()
        ctx.stroke ()
        ctx.restore ()

        ctx |> node "a" 250. kleisli LeftUnder (stagger 2 1 tl.["init"])
        ctx |> node "b" 600. kleisli RightUnder (stagger 2 1 tl.["init"])
        ctx.arrow (285., kleisli, 565., kleisli, -50., tl.["a->b"])

        let d = sqrt (45.**2. / 2.)
        ctx.arcArrow (600. + d, kleisli - d, 25., Math.PI, (1. + 1.5 * tl.["id"]) * Math.PI)

        let label = "a -> Writer b"
        ctx.font <- codeFont 40
        ctx.lineWidth <- 1.
        ctx.drawText (label, 425. - ctx.measureText(label).width / 2., kleisli - 50., tl.["a->b"])

        ctx.drawText ("b ->", 600. + d + 45., kleisli - d, tl.["id"])
        ctx.drawText ("Writer b", 600. + d + 45., kleisli - d + ctx.actualLineHeight, tl.["id"])
    )
}

let v03_kleisli =
    [ kleisliWriter ]

let scenes = v03_kleisli
