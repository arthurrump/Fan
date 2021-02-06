module CTP.V4

open CTP.Shared
open System
open Browser.Types
open Animation.Animation
open Canvas

let writerI = scene "writerI" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]

        ctx.textBaseline <- "top"
        ctx.font <- codeFont 65
        ctx.drawLongText([
            [ style keyword "interface "; style typeDecl "Writer"; style text "<"; style typeDecl "A"; style text "> {" ]
            [ style var "  res"; style text ": "; style typeDecl "A"; style text ";" ]
            [ style var "  logger"; style text ": "; style typeDecl "string"; style text ";" ]
            [ style text "}" ]
        ], 100., ctx.height / 2. - 2. * ctx.actualLineHeight, tl.["init"])

        ctx |> drawLanguageIndicator "TypeScript" tl.["init"]
    )
}

let listMap = scene "listMap" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
        0 => fadeIn 750 Linear "long"
    })
    run (animation {
        0 => fadeOut 750 Linear "long"
        250 => fadeIn 750 Linear "short"
        500 => fadeIn 750 EaseInOutQuad "pos"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.textBaseline <- "top"
        ctx.font <- codeFont 65
        let textY = interpolate (ctx.height / 2. - 1.5 * ctx.actualLineHeight) 100. tl.["pos"]
        let text = color "#fff"

        ctx.drawLongText([
            [ style text "List.map" ]
        ], 100., textY, tl.["init"])

        ctx.drawLongText([
            [ ]
            [ style keyword "  (fun "; style var "x"; style keyword " -> "; style text "x"; style keyword " * "; style text "x"; style keyword ")" ]
            [ style keyword "  [ "; style numberLit "0"; style keyword " .. "; style numberLit "10"; style keyword " ]" ]
        ], 100., textY, tl.["long"])

        ctx.drawLongText([
            [ style text "         f list" ]
        ], 100., textY, tl.["short"])


        ctx.globalAlpha <- tl.["opacity"]
        ctx |> drawLanguageIndicator "F#" tl.["init"]
    )
}

let listFunctor = scene "listFunctor" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
    })
    run (animation {
        0 => fadeIn 750 EaseOutQuad "func_f" // 1:19
        9500 => fadeIn 750 EaseOutQuad "functor" // 1:28.5
        13000 => fadeIn 750 Linear "cat_list" // 1:32
        67000 => fadeIn 750 EaseOutQuad "func_mapf" // 2:26
        85000 => fadeIn 750 Linear "func_mapf_math" // 2:44
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]
        ctx.textBaseline <- "top"
        ctx.font <- codeFont 65
        ctx.setStyle (color "#fff")
        ctx.fillText ("List.map f list", 100., 100.)

        let top = ctx.height * (1./3.)
        let middle = ctx.height * (1./2.) + 50.
        let bottom = ctx.height * (2./3.) + 100.

        ctx.nodeSize <- 15.
        ctx.lineWidth <- 3.

        ctx.cloud (color "#def5") 400. bottom 200. 100. tl.["init"]
        ctx.node ("a", 300., bottom, LeftUnder, tl.["init"], 55)
        ctx.node ("b", 500., bottom - 15., RightUnder, tl.["func_f"], 55)

        ctx.nodeArrow (300., bottom, 500., bottom - 15., -20., tl.["func_f"])
        ctx.save ()
        ctx.lineWidth <- 1.
        ctx.font <- mathFont 55
        ctx.textBaseline <- "alphabetic"
        let func = "f"
        let funcWidth = ctx.measureText(func).width
        ctx.drawText (func, 400. - 0.5 * funcWidth, bottom - 30., tl.["func_f"])
        ctx.restore ()

        ctx.save ()
        ctx.lineWidth <- 5.
        ctx.beginPath ()
        ctx.blockArrow (400., middle - 30., 60., 80., tl.["functor"])
        ctx.stroke ()
        ctx.lineWidth <- 1.
        ctx.font <- serifFont 65
        ctx.textBaseline <- "center"
        ctx.drawText ("List", 450., middle - 10., tl.["functor"])
        ctx.restore ()

        ctx.cloud (color "#fdf5") 400. top 250. 120. tl.["cat_list"]
        ctx.node ("List a", 260., top, CenterUnder, tl.["cat_list"], 45)
        ctx.node ("List b", 550., top - 15., CenterUnder, tl.["cat_list"], 45)
        
        ctx.nodeArrow (260., top, 550., top - 15., -20., tl.["func_mapf"])
        ctx.save ()
        ctx.lineWidth <- 1.
        ctx.textBaseline <- "alphabetic"
        ctx.font <- codeFont 40
        let code = "List.map"
        let codeWidth = ctx.measureText(code).width
        ctx.font <- mathFont 50
        let func = " " + func
        let funcWidth = ctx.measureText(func).width
        ctx.font <- serifFont 45
        let functor = "List"
        let functorWidth = ctx.measureText(functor).width
        let codeX = 400. - 0.5 * (interpolate codeWidth functorWidth tl.["func_mapf_math"] + funcWidth)
        let funcX = codeX + (interpolate codeWidth functorWidth tl.["func_mapf_math"])
        ctx.font <- codeFont 40
        ctx.drawText (code, codeX, top - 30., tl.["func_mapf"] - tl.["func_mapf_math"])
        ctx.font <- mathFont 50
        ctx.drawText (func, funcX, top - 30., tl.["func_mapf"])
        ctx.font <- serifFont 45
        ctx.drawText (functor, codeX, top - 30., tl.["func_mapf_math"])
        ctx.restore ()
    )
}

let functorPreservesStructure = scene "functorPreservesStructure" {
    enter (animation {
        0 => fadeIn 750 EaseOutQuad "init"
    })
    run (animation {
        0 => fadeIn 750 EaseOutQuad "h" // 4:01.5
        2500 => fadeIn 750 EaseOutQuad "functor" // 4:04
        2750 => fadeIn 750 Linear "catF" // Fini at 4:05.25
        3000 => fadeIn 750 EaseOutQuad "catF_funcs"
        4000 => fadeIn 750 EaseOutQuad "Fh" // 4:05.5
        7000 => fadeIn 750 Linear "Fh_comp" // 4:08.5
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]
        let top = ctx.height * (1./3.) - 100.
        let middle = ctx.height * (1./2.)
        let bottom = ctx.height * (2./3.) + 100.

        ctx.setStyle (color "#fff")
        ctx.nodeSize <- 15.
        ctx.lineWidth <- 3.

        ctx.cloud (color "#def5") 400. bottom 270. 160. tl.["init"]
        ctx.node ("a", 220., bottom, LeftAbove, tl.["init"], 55)
        ctx.node ("b", 415., bottom + 75., CenterUnder, tl.["init"], 55)
        ctx.node ("c", 560., bottom - 20., RightAbove, tl.["init"], 55)
        ctx.nodeArrow (220., bottom, 415., bottom + 75., -10., tl.["init"])
        ctx.textBaseline <- "top"
        ctx.label ("", "f", 305., bottom + 45., tl.["init"])
        ctx.nodeArrow (415., bottom + 75., 560., bottom - 20., -15., tl.["init"])
        ctx.label ("", "g", 492., bottom + 20., tl.["init"])
        ctx.nodeArrow (220., bottom, 560., bottom - 20., -50., tl.["h"])
        ctx.textBaseline <- "alphabetic"
        ctx.label ("", "h", 390., bottom - 50., tl.["h"])

        ctx.save ()
        ctx.lineWidth <- 5.
        ctx.beginPath ()
        ctx.blockArrow (400., middle - 40., 60., 80., tl.["functor"])
        ctx.stroke ()
        ctx.lineWidth <- 1.
        ctx.font <- serifFont 65
        ctx.textBaseline <- "middle"
        ctx.drawText ("F", 450., middle + 10., tl.["functor"])
        ctx.restore ()

        ctx.cloud (color "#fdf5") 400. top 270. 160. tl.["catF"]
        ctx.node ("Fa", 220., top, CenterAbove, tl.["catF"], 55)
        ctx.node ("Fb", 415., top + 75., CenterUnder, tl.["catF"], 55)
        ctx.node ("Fc", 560., top - 20., CenterAbove, tl.["catF"], 55)
        ctx.nodeArrow (220., top, 415., top + 75., -10., tl.["catF_funcs"])
        ctx.textBaseline <- "top"
        ctx.label ("F", "f", 305., top + 50., tl.["catF_funcs"])
        ctx.nodeArrow (415., top + 75., 560., top - 20., -15., tl.["catF_funcs"])
        ctx.label ("F", "g", 510., top + 30., tl.["catF_funcs"])
        ctx.nodeArrow (220., top, 560., top - 20., -50., tl.["Fh"])
        ctx.textBaseline <- "alphabetic"
        ctx.label ("F", "h", 390., top - 50., tl.["Fh"])

        ctx.textBaseline <- "middle"
        let s = serifFont 55
        let m = mathFont 60
        ctx.font <- m
        ctx.lineWidth <- 1.
        ctx.drawText ("h = g \u2218 f", 800., bottom, tl.["h"])
        ctx.drawLongText ([[ font s "F"; font m "h = "; font s "F"; font m "g \u2218 "; font s "F"; font m "f" ]], 800., top, tl.["Fh_comp"])
    )
}

let functorPreservesIdentity = scene "functorPreservesIdentity" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
        500 => fadeIn 750 EaseOutQuad "id_a"
    })
    run (animation {
        0 => fadeIn 750 Linear "functor" // 4:31.5
        500 => fadeIn 750 Linear "catF" // 4:32
        8500 => fadeIn 750 EaseOutQuad "id_Fa" // 4:40
        17500 => fadeIn 750 Linear "id_eq" // 4:49
    })
    leave (animation {
        0 => fadeOut 750 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]
        ctx.setStyle (color "#fff")
        ctx.nodeSize <- 15.
        ctx.lineWidth <- 3.
        ctx.font <- serifFont 55
        let d = sqrt (30.**2. / 2.)
        let top = ctx.height * (1./3.) - 100.
        let middle = ctx.height * (1./2.)
        let bottom = ctx.height * (2./3.) + 100.
        let idLabel fz x y sub progress =
            let id' = "id"
            let idWidth = ctx.measureText(id').width
            ctx.save ()
            ctx.lineWidth <- 1.
            ctx.font <- mathFont fz
            ctx.drawText(id', x, y, staggeredProgress 0.5 2 0 progress)
            ctx.font <- serifFont (int (0.75 * float fz))
            let subWidth = ctx.measureText(sub).width
            ctx.drawText(sub, x + idWidth, y + 10., staggeredProgress 0.5 2 1 progress)
            ctx.restore ()
            idWidth + subWidth
        let drawTextWidth text x y progress =
            ctx.drawText (text, x, y, progress)
            ctx.measureText(text).width
        
        ctx.cloud (color "#def5") 400. bottom 150. 150. tl.["init"]
        ctx.node ("a", 370., bottom + 20., LeftUnder, tl.["init"], 55)
        ctx.arcArrow (370. + d, bottom + 20. - d, 20., Math.PI, (1. + 1.5 * tl.["id_a"]) * Math.PI)
        idLabel 55 (370. + d) (bottom - 2. * d) "a" tl.["id_a"] |> ignore

        ctx.save ()
        ctx.lineWidth <- 5.
        ctx.beginPath ()
        ctx.blockArrow (400., middle - 40., 60., 80., tl.["functor"])
        ctx.stroke ()
        ctx.lineWidth <- 1.
        ctx.font <- serifFont 65
        ctx.textBaseline <- "middle"
        ctx.drawText ("F", 450., middle + 10., tl.["functor"])
        ctx.restore ()

        ctx.cloud (color "#fdf5") 400. top 150. 150. tl.["catF"]
        ctx.node ("Fa", 370., top + 20., LeftUnder, tl.["catF"], 55)
        ctx.arcArrow (370. + d, top + 20. - d, 20., Math.PI, (1. + 1.5 * tl.["id_Fa"]) * Math.PI)
        idLabel 55 (370. + d) (top - 2. * d) "Fa" tl.["id_Fa"] |> ignore

        ctx.font <- serifFont 70
        let s = staggeredProgress 0.5 4
        let y = top - 2. * d
        let x = 650.
        let x = x + idLabel 70 x y "Fa" (s 0 tl.["id_eq"])
        let x = x + drawTextWidth " = " x y (s 1 tl.["id_eq"])
        let x = x + drawTextWidth "F" x y (s 2 tl.["id_eq"])
        let x = x + idLabel 70 x y "a" (s 3 tl.["id_eq"])
        ()
    )
}

let functorComposition = scene "functorComposition" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
    })
    run (animation {
        0 => fadeIn 750 EaseOutQuad "functor"
        500 => fadeIn 750 Linear "cat_Writer"
        1000 => fadeIn 750 EaseOutQuad "cat_Writer_f"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]

        let top = ctx.height * (1./3.) - 50.
        let middle = ctx.height * (1./2.)
        let bottom = ctx.height * (2./3.) + 50.

        ctx.setStyle (color "#fff")
        ctx.nodeSize <- 15.
        ctx.lineWidth <- 3.

        ctx.cloud (color "#fdf5") 400. bottom 250. 120. tl.["init"]
        ctx.node ("List a", 260., bottom, CenterUnder, tl.["init"], 45)
        ctx.node ("List b", 550., bottom - 15., CenterUnder, tl.["init"], 45)
        
        ctx.nodeArrow (260., bottom, 550., bottom - 15., -20., tl.["init"])
        ctx.save ()
        ctx.lineWidth <- 1.
        ctx.textBaseline <- "alphabetic"
        ctx.drawLongText ([[ font (serifFont 45) "List "; font (mathFont 50) "f" ]], 340., bottom - 30., tl.["init"])
        ctx.restore ()

        ctx.save ()
        ctx.lineWidth <- 5.
        ctx.beginPath ()
        ctx.blockArrow (400., middle - 40., 60., 80., tl.["functor"])
        ctx.stroke ()
        ctx.lineWidth <- 1.
        ctx.font <- serifFont 65
        ctx.textBaseline <- "middle"
        ctx.drawText ("Writer", 450., middle + 10., tl.["functor"])
        ctx.restore ()

        ctx.cloud (color "#efe5") 400. top 300. 120. tl.["cat_Writer"]
        ctx.node ("Writer List a", 260., top, CenterUnder, tl.["cat_Writer"], 45)
        ctx.node ("Writer List b", 550., top - 15., CenterUnder, tl.["cat_Writer"], 45)

        ctx.nodeArrow (260., top, 550., top - 15., -20., tl.["cat_Writer_f"])
        ctx.save ()
        ctx.lineWidth <- 1.
        ctx.textBaseline <- "alphabetic"
        ctx.drawLongText ([[ font (serifFont 45) "Writer List "; font (mathFont 50) "f" ]], 270., top - 40., tl.["cat_Writer_f"])
        ctx.restore ()
    )
}

let scenes =
    [ writerI; listMap; listFunctor; functorPreservesStructure; functorPreservesIdentity; functorComposition ]
