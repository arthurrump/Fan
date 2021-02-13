module CTP.V2

open CTP.Shared
open System
open Browser.Types
open Fan.Animation
open Fan.Canvas

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
        5000 => fadeIn 750 EaseOutQuad "composition"
        17000 => fadeIn 750 EaseOutQuad "composition_arrow"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]

        let middle = ctx.height / 3.
        ctx.node ("", 200., middle, CenterUnder, tl.["init"])
        ctx.node ("", 450., middle, CenterUnder, tl.["init"])
        ctx.node ("", 700., middle, CenterUnder, tl.["init"])

        ctx.strokeStyle <- color "#fff"
        ctx.lineWidth <- 5.

        ctx.arrow (235., middle, 415., middle, 0., stagger 2 0 tl.["initialArrows"])
        ctx.arrow (485., middle, 665., middle, 0., stagger 2 1 tl.["initialArrows"])

        ctx.arcArrow (200., middle + 45., 25., -0.2 * Math.PI, (-0.2 + 1.4 * tl.["identity"]) * Math.PI)
        ctx.arcArrow (450., middle + 45., 25., -0.2 * Math.PI, (-0.2 + 1.4 * tl.["identity"]) * Math.PI)
        ctx.arcArrow (700., middle + 45., 25., -0.2 * Math.PI, (-0.2 + 1.4 * tl.["identity"]) * Math.PI)

        ctx.arrow (200., middle - 35., 700., middle - 35., -75., tl.["composition_arrow"])

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
            [ style keyword "function "; style funcDecl "toInt"; style text "("; style var "value"; style text ") {" ]
            [ style control "  return "; style var "value"; style operator " ? "; style numberLit "1"; style operator " : "; style numberLit "0"; style text ";" ]
            [ style text "}" ]
        ], 100., 100., tl.["init"])

        ctx.drawLongText([
            [ style keyword "function "; style funcDecl "toString"; style text "("; style var "value"; style text ") {" ]
            [ style control "  return "; style var "value"; style text "."; style funcDecl "toString"; style text "();" ]
            [ style text "}" ]
        ], 100., 100. + 4. * ctx.actualLineHeight, tl.["init"])

        ctx.drawLongText([
            [ style funcDecl "toString"; style text "("; style funcDecl "toInt"; style text "("; style keyword "true"; style text "));" ]
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
        23500 => fadeIn 750 Linear "mathComposition"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]
        ctx.font <- codeFont 65
        ctx.textBaseline <- "top"

        ctx.drawLongText([
            [ style funcDecl "toInt "; style operator ":: "; style typeDecl "Bool"; style operator " -> "; style typeDecl "Int" ]
            [ style text "toInt False = "; style numberLit "0" ]
            [ style text "toInt True = "; style numberLit "1" ]
        ], 100., 100., tl.["init"])

        ctx.drawLongText([
            [ style funcDecl "toString "; style operator ":: "; style typeDecl "Int"; style operator " -> "; style typeDecl "String" ]
            [ style text "toString value = show value" ]
        ], 100., 100. + 4. * ctx.actualLineHeight, tl.["init"])

        ctx.drawLongText([
            [ style funcDecl "boolToString "; style operator ":: "; style typeDecl "Bool"; style operator " -> "; style typeDecl "String" ]
            [ style text "boolToString "]
        ], 100., 100. + 7. * ctx.actualLineHeight, tl.["init"])

        ctx.drawLongText([
            [ style text "             value = toString (toInt value)" ]
        ], 100., 100. + 8. * ctx.actualLineHeight, tl.["composition"])

        ctx.drawLongText([
            [ style text "             = toString . toInt" ]
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

let typesCategory = scene "typesCategory" {
    enter (animation {
        0 => fadeIn 750 Linear "objects"
    })
    run (animation {
        0 => fadeIn 750 EaseOutQuad "identity" // 3:32,5
        4500 => fadeIn 750 EaseOutQuad "absurd_bool" // 3:37
        9000 => fadeIn 750 EaseOutQuad "ignore_bool" // 3:41,5
        12500 => fadeIn 750 EaseOutQuad "pick_bool" // 3:45
        19500 => fadeIn 750 Linear "highlight_void_unit_path" // 3:52
        29500 => fadeIn 750 EaseOutQuad "absurd_unit" // 4:02
        34000 => fadeOut 500 Linear "highlight_void_unit_path" // 4:06,5
        46500 => fadeIn 750 Linear "highlight_bool_konst" // 4:19
        59000 => fadeOut 500 Linear "highlight_bool_konst" // 4:33,5
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]
        ctx.node ("Bool", 250., 700., LeftUnder, tl.["objects"])
        ctx.node ("Unit", 800., 700., RightUnder, tl.["objects"])
        ctx.node ("Void", 525., 300., CenterAbove, tl.["objects"])

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

        // 5:09,5
        for f in [ "f"; "g"; "h"] do 
            0 => fadeIn 750 Linear $"hi_{f}"
        
        // 5:21
        11500 => fadeOut 500 Linear "hi_f"
        11500 => fadeOut 500 Linear "hi_g"
        11500 => fadeIn 750 Linear "hi_fg"

        19750 => fadeIn 750 Linear "hi_f"
        19750 => fromTo 0. 0.5 750 Linear "hi_g"
        19750 => fadeOut 500 Linear "hi_fg"
        // 5:30
        20500 => fadeOut 500 Linear "hi_g"
        20500 => fadeOut 500 Linear "hi_h"
        20500 => fadeIn 750 Linear "hi_gh"

        // 5:39
        29500 => fadeOut 500 Linear "hi_f"
        29500 => fadeOut 500 Linear "hi_gh"
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
        ctx.node ("", 200., middle - 200., CenterUnder, tl.["init"])
        ctx.node ("", 600., middle - 200., CenterUnder, tl.["init"])
        ctx.node ("", 600., middle + 200., CenterUnder, tl.["init"])
        ctx.node ("", 200., middle + 200., CenterUnder, tl.["init"])

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
            [ (Some (rgba (cp t gh tl.["hi_gh"])), None, "(")
              (Some (rgba (cp (cp w h tl.["hi_h"]) gh tl.["hi_gh"])), None, "h") 
              (Some (rgba (cp w gh tl.["hi_gh"])), None, " \u2218")
              (Some (rgba (cp t fg tl.["hi_fg"])), None, "(")
              (Some (rgba (cp (cp (cp w g tl.["hi_g"]) gh tl.["hi_gh"]) fg tl.["hi_fg"])), None, "g")
              (Some (rgba (cp t gh tl.["hi_gh"])), None, ")")
              (Some (rgba (cp w fg tl.["hi_fg"])), None, "\u2218 ")
              (Some (rgba (cp (cp w f tl.["hi_f"]) fg tl.["hi_fg"])), None, "f")
              (Some (rgba (cp t fg tl.["hi_fg"])), None, ")") ]
        ], 750., middle, tl.["mathEnter"])
    )
}

let identity = scene "identity" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
        750 => fadeIn 750 Linear "hi_f"
    })
    run (animation {
        // 5:51
        0 => fadeIn 750 Linear "f_id"
        0 => fadeIn 750 Linear "hi_id_f"

        // 5:56,5
        5500 => fadeOut 500 Linear "hi_id_f"

        // 5:59
        8000 => fadeIn 750 Linear "id_f"
        8000 => fadeIn 750 Linear "hi_f_id"

        // 6:04,5
        13500 => fadeOut 500 Linear "hi_f_id"
        13500 => fadeOut 500 Linear "hi_f"
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
        ctx.node ("", 200., middle - 200., CenterUnder, 1.)
        ctx.node ("", 600., middle - 200., CenterUnder, 1.)
        ctx.node ("", 600., middle + 200., CenterUnder, 1.)
        ctx.node ("", 200., middle + 200., CenterUnder, 1.)

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
        ctx.drawText("f \u2218 id = f", 750., middle - ctx.actualLineHeight, tl.["f_id"])
        ctx.drawText("id \u2218 f = f", 750., middle, tl.["id_f"])
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

let scenes =
    [ composition; compositionText; simpleCategory; jsComposition; haskComposition
      typesCategory; compositionRules; associativity; identity; categorySet ]
