module CTP

open System
open Browser.Types
open Animation
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
let mathFont size = sprintf "%ipx 'CMU Serif', math" size
let codeFont size = sprintf "%ipx Consolas, monospace" size

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
    // TODO: text morph
    run (animation {
        0 => fadeIn 1000 Linear "code"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
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
        0 => fadeIn 1000 Linear "init"
    })
    run (animation {
        // TODO: create some sort of background animation thing for this
        timeline' (Alternate, Infinite) {
            0 => vars { "seed" => 1.1 }
            5000 => vars { "seed" => 1.4 }
        }

        2000 => fadeIn 1000 EaseOutCubic "intbool"
        4000 => fadeIn 1000 EaseInCubic "intboolout"

        6000 => fadeIn 1000 EaseOutCubic "boolint"
        8000 => fadeIn 1000 EaseInCubic "boolintout"
    })
    leave (animation {
        0 => fadeOut 1000 Linear "init"
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
        0 => fadeIn 1000 Linear "init"
    })
    leave (animation {
        0 => fadeOut 1000 Linear "opacity"
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
        0 => fadeIn 1000 Linear "init"
    })
    run (animation {
        timeline' (Alternate, Infinite) {
            0 => vars { "seed" => 1.4 }
            4000 => vars { "seed" => 2.0 }
        }

        1000 => fadeIn 1000 EaseOutCubic "ignore"
        3000 => fadeIn 1000 EaseInCubic "ignoreout"

        5000 => fadeIn 1000 EaseOutCubic "absurd"
        7000 => fadeIn 1000 EaseInCubic "absurdout"

        9000 => fadeIn 1000 EaseOutCubic "id"
        11000 => fadeIn 1000 EaseInCubic "idout"
    })
    leave (animation {
        0 => fadeOut 1000 Linear "init"
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
        ctx.arcArrow (240., 700., 70., 0.1 * Math.PI + 1.8 * Math.PI * tl.["idout"], 0.1 * Math.PI + 1.8 * Math.PI * tl.["id"])
        ctx.arcArrow (770., 700., 70., -0.9 * Math.PI + 1.8 * Math.PI * tl.["idout"], -0.9 * Math.PI + 1.8 * Math.PI * tl.["id"])
        ctx.restore ()
    )
}

let sideEffect = scene "sideEffect" {
    enter (animation {
        0 => fadeIn 1000 Linear "init"
    })
    leave (animation {
        0 => fadeOut 1000 Linear "opacity"
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

let scenes =
    [ intro  
      // 1: Types and functions
      types; boolSet; intSet; intRange; stringSet; unitSet; voidIsUnit; voidSet; voidOO
      typeSetsFunctions; haskellToInt; setDefinesFunc
      // 2: Why types?
      sideEffect ]