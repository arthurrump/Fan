module CTP.V1

open CTP.Shared
open System
open Browser.Types
open Fan.Animation
open Fan.Canvas

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
            [ style operator "[ "; style numberLit "−2147483648" ] 
            [ style operator " .. "; style numberLit "2147483647"; style operator " ]" ]
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
        
        ctx.drawLongText ([ [ style text "("; style keyword "void"; style text ")" ] ], 500., 200., tl.["void"])

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
            [ style keyword "public sealed class "; style typeDecl "Unit" ] 
            [ style operator "{" ]
            [ style keyword "  public static "; style typeDecl "Unit "; style var "Instance"; style operator " = "; 
              style keyword "new "; style typeDecl "Unit"; style operator "();" ]
            [ style keyword "  private "; style funcDecl "Unit"; style operator "() { }" ]
            [ style operator "}" ]
        ], 100., 100., tl.["class_unit"])

        ctx.drawLongText([
            [ style keyword "     "; style funcDecl "Ignore"; style operator "<"; style typeDecl "T"; style operator ">("
              style typeDecl "T "; style var "value"; style operator ")" ]
            [ style operator "{" ]
            [ style keyword "  "; style control "return"; ]
            [ style operator "}" ]
        ], 100., 520., tl.["code"])

        ctx.drawLongText([
            [ style keyword "void" ]
            [ ]
            [ style text "        "; style operator ";" ]
        ], 100., 520., tl.["code_void"])

        ctx.drawLongText([
            [ style typeDecl "Unit" ]
            [ ]
            [ style text "         "; style var "Unit"; style operator "."; style var "Instance"; style operator ";" ]
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
            [ style keyword "public sealed class "; style typeDecl "Void" ]
            [ style operator "{" ]
            [ style keyword "  private "; style funcDecl "Void"; style operator "() { }" ]
            [ style operator "}" ]
        ], 100., 350., tl.["code"])
        
        ctx |> drawLanguageIndicator "C#" tl.["code"]
    )
}

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
            [ style funcDecl "toInt "; style operator ":: "; style typeDecl "Bool"; style operator " -> "; style typeDecl "Int" ]
            [ style text "toInt False = "; style numberLit "0" ]
            [ style text "toInt True = "; style numberLit "1" ]
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
            [ style keyword "public "; style typeDecl "double "; style funcDecl "Square"; style text "("
              style typeDecl "double "; style text "x) {" ]
            [ style text "  System.out."; style funcDecl "println"; style text "("; style stringLit "\"Squaring \""; style text " + x);" ]
            [ style control "  return "; style text "x * x;" ]
            [ style text "}" ]
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

let scenes = 
    [ types; boolSet; intSet; intRange; stringSet; unitSet; voidIsUnit; voidSet; voidOO
      typeSetsFunctions; haskellToInt; setDefinesFunc
      sideEffect
      whyTypes ]
