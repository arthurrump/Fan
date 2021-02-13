module CTP.V5

open CTP.Shared
open System
open Browser.Types
open Fan.Animation
open Fan.Canvas

let monad = scene "monad" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
        250 => fadeIn 750 EaseOutQuad "init_funcs"
        500 => fadeIn 750 EaseOutQuad "functor"
        1000 => fadeIn 750 Linear "catF"
        1250 => fadeIn 750 EaseOutQuad "catF_funcs"
    })
    run (animation {
        0 => fadeIn 750 Linear "subcat" // 0:30
        17500 => fadeIn 750 EaseOutQuad "catK" // 0:47.5
        23000 => fadeIn 750 Linear "catK_fl" // 0:53
        38500 => fadeIn 750 EaseOutQuad "monad" // 1:08.5
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]
        let d = sqrt (30.**2. / 2.)
        let top = ctx.height * (1./3.) - 100.
        let middle = ctx.height * (1./2.)
        let bottom = ctx.height * (2./3.) + 100.

        ctx.setStyle (color "#fff")
        ctx.nodeSize <- 15.
        ctx.lineWidth <- 3.
        ctx.textBaseline <- "top"

        ctx.cloud (color "#ddd5") 350. middle 325. (ctx.height / 2. - 25.) tl.["subcat"]

        ctx.cloud (color "#def5") 350. bottom 200. 150. (tl.["init"] - 0.8 * tl.["subcat"])
        ctx.node ("a", 245., bottom - 20., LeftAbove, tl.["init"], 55)
        ctx.node ("b", 440., bottom + 30., CenterUnder, tl.["init"], 55)
        ctx.nodeArrow (245., bottom - 20., 440., bottom + 30., -40., tl.["init_funcs"])
        ctx.label ("", "f", 330., bottom + 10., tl.["init"])
        ctx.arcArrow (245. - d, bottom - 20. + d, 20., 1.5 * Math.PI, (1.5 - 1.5 * tl.["init_funcs"]) * Math.PI, true)
        ctx.arcArrow (440. + d, bottom + 30. - d, 20., Math.PI, (1. + 1.5 * tl.["init_funcs"]) * Math.PI)

        ctx.save ()
        ctx.lineWidth <- 5.
        ctx.beginPath ()
        ctx.blockArrow (350., middle - 40., 60., 80., tl.["functor"])
        ctx.stroke ()
        ctx.lineWidth <- 1.
        ctx.font <- serifFont 65
        ctx.textBaseline <- "middle"
        ctx.drawText ("F", 400., middle + 10., tl.["functor"])
        ctx.restore ()

        ctx.cloud (color "#fdf5") 350. top 200. 150. (tl.["catF"] - 0.8 * tl.["subcat"])
        ctx.node ("Fa", 245., top - 20., CenterAbove, tl.["catF"], 55)
        ctx.node ("Fb", 440., top + 30., CenterUnder, tl.["catF"], 55)
        ctx.nodeArrow (245., top - 20., 440., top + 30., -40., tl.["catF_funcs"])
        ctx.label ("F", "f", 330., top + 15., tl.["catF_funcs"])
        ctx.arcArrow (245. - d, top - 20. + d, 20., 1.5 * Math.PI, (1.5 - 1.5 * tl.["catF_funcs"]) * Math.PI, true)
        ctx.arcArrow (440. + d, top + 30. - d, 20., Math.PI, (1. + 1.5 * tl.["catF_funcs"]) * Math.PI)

        ctx.save ()
        ctx.font <- serifFont 65
        ctx.lineWidth <- 1.
        ctx.drawText ("Kleisli", 1200., 100., tl.["catK"])
        ctx.restore ()
        
        ctx.cloud (color "#efe5") 1020. top 250. 135. (tl.["catK"])
        ctx.node ("a", 895., top - 20., LeftAbove, tl.["catK"], 55)
        ctx.node ("b", 1090., top + 30., CenterUnder, tl.["catK"], 55)
        ctx.nodeArrow (895., top - 20., 1090., top + 30., -40., tl.["catK"])
        ctx.arcArrow (895. - d, top - 20. + d, 20., 1.5 * Math.PI, (1.5 - 1.5 * tl.["catK"]) * Math.PI, true)
        ctx.arcArrow (1090. + d, top + 30. - d, 20., Math.PI, (1. + 1.5 * tl.["catK"]) * Math.PI)
        ctx.save ()
        ctx.lineWidth <- 1.
        ctx.lineHeight <- 1.
        ctx.font <- serifFont 40
        ctx.textBaseline <- "bottom"
        ctx.drawText ("a \u2192 Fb", 930., top - 20., tl.["catK_fl"])
        ctx.drawText ("b \u2192", 1090. + 2.5 * d, top + 30. - d, tl.["catK_fl"])
        ctx.drawText (" Fb", 1090. + 2.5 * d, top + 30. - d + ctx.actualLineHeight, tl.["catK_fl"])
        ctx.textBaseline <- "top"
        ctx.drawText ("a \u2192 Fa", 840., top - 20. + 2.5 * d, tl.["catK_fl"])
        ctx.restore ()

        ctx.save ()
        ctx.arrow (520., 610., 460., middle, 50., tl.["monad"])
        ctx.lineWidth <- 1.
        ctx.font <- serifFont 55
        ctx.drawText ("monad", 460., 610., tl.["monad"])
        ctx.restore ()
    )
}

let natTransf = scene "natTransf" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
        250 => fadeIn 750 EaseOutQuad "functor"
        500 => fadeIn 750 EaseOutQuad "natTransf"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl -> 
        ctx.globalAlpha <- tl.["opacity"]
        let middle = ctx.height / 2.
        ctx.setStyle (color "#fff")
        ctx.lineWidth <- 3.
        
        ctx.node ("C", 200., middle, LeftUnder, tl.["init"])
        ctx.node ("D", 500., middle, RightUnder, tl.["init"])
        ctx.nodeArrow (200., middle, 500., middle, -150., tl.["functor"])
        ctx.nodeArrow (200., middle, 500., middle, 150., tl.["functor"])

        ctx.beginPath ()
        ctx.blockArrow (350., middle + 40., 40., 80., tl.["natTransf"], Math.PI)
        ctx.stroke ()

        ctx.lineWidth <- 1.
        ctx.font <- serifFont 60
        ctx.textBaseline <- "middle"
        ctx.drawText ("\u03b1", 380., middle, tl.["natTransf"])
    )
}

let product = scene "product" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
        500 => fadeIn 750 EaseOutQuad "f"
    })
    run (animation {
        0 => fadeIn 750 EaseOutQuad "m"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]
        let middle = ctx.height / 2.
        ctx.style <- color "#fff"
        ctx.nodeSize <- 20.
        ctx.lineWidth <- 3.

        ctx.node ("", 280., middle + 150., CenterUnder, tl.["init"])
        ctx.node ("", 540., middle + 140., CenterUnder, tl.["init"])

        ctx.node ("", 400., middle - 40., CenterUnder, tl.["init"])
        ctx.nodeArrow (400., middle - 40., 280., middle + 150., 50., tl.["f"])
        ctx.nodeArrow (400., middle - 40., 540., middle + 140., -60., tl.["f"])
        
        ctx.node ("", 600., middle, CenterUnder, tl.["init"])
        ctx.nodeArrow (600., middle, 280., middle + 150., 50., tl.["f"])
        ctx.nodeArrow (600., middle, 540., middle + 140., -60., tl.["f"])
        
        ctx.node ("", 300., middle - 100., CenterUnder, tl.["init"])
        ctx.nodeArrow (300., middle - 100., 280., middle + 150., 50., tl.["f"])
        ctx.nodeArrow (300., middle - 100., 540., middle + 140., -180., tl.["f"])

        ctx.node ("", 500., middle - 140., CenterUnder, tl.["init"])
        ctx.nodeArrow (500., middle - 140., 280., middle + 150., 140., tl.["f"])
        ctx.nodeArrow (500., middle - 140., 540., middle + 140., -60., tl.["f"])

        ctx.nodeArrow (600., middle, 400., middle - 40., 20., tl.["m"])
        ctx.nodeArrow (300., middle - 100., 400., middle - 40., 40., tl.["m"])
        ctx.nodeArrow (500., middle - 140., 400., middle - 40., -20., tl.["m"])
    )
}

let homset = scene "homset" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
        500 => fadeIn 750 EaseOutQuad "f"
    })
    run (animation {
        0 => fadeIn 750 EaseOutQuad "homset"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]
        let middle = ctx.height / 2.
        ctx.style <- color "#fff"
        ctx.nodeSize <- 20.
        ctx.lineWidth <- 3.

        ctx.cloud (color "#def5") 350. middle 130. 100. tl.["homset"]

        ctx.node ("", 200., middle, Left, tl.["init"])
        ctx.node ("", 500., middle, Left, tl.["init"])
        ctx.nodeArrow (200., middle, 500., middle, 160., tl.["f"])
        ctx.nodeArrow (200., middle, 500., middle, 80., tl.["f"])
        ctx.nodeArrow (200., middle, 500., middle, 0., tl.["f"])
        ctx.nodeArrow (200., middle, 500., middle, -80., tl.["f"])
        ctx.nodeArrow (200., middle, 500., middle, -160., tl.["f"])

        ctx.arrow (450., middle + 180., 350., middle + 130., -50., tl.["homset"])
        ctx.lineWidth <- 1.
        ctx.textBaseline <- "middle"
        ctx.font <- serifFont 55
        ctx.drawText ("hom-set", 460., middle + 180., tl.["homset"])
    )
}

let oppositeCat = scene "oppositeCat" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
        500 => fadeIn 750 EaseOutQuad "f"
    })
    run (animation {
        0 => fadeIn 750 Linear "catOp"
        500 => fadeIn 750 EaseOutQuad "catOp_f"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]
        let top = ctx.height * (1./3.) - 50.
        let bottom = ctx.height * (2./3.) + 50.
        ctx.style <- color "#fff"
        ctx.nodeSize <- 20.
        ctx.lineWidth <- 3.
        let d = sqrt (35.**2. / 2.)

        ctx.cloud (color "#def5") 390. (bottom + 20.) 260. 140. tl.["init"]
        ctx.node ("", 220., bottom, LeftAbove, tl.["init"], 55)
        ctx.arcArrow (220. - d, bottom - d, 20., 0., (-1.5 * tl.["f"]) * Math.PI, true)
        ctx.node ("", 415., bottom + 75., CenterUnder, tl.["init"], 55)
        ctx.arcArrow (415. + d, bottom + 75. + d, 20., 1.5 * Math.PI, (1.5 + 1.5 * tl.["f"]) * Math.PI)
        ctx.node ("", 560., bottom - 20., RightAbove, tl.["init"], 55)
        ctx.arcArrow (560. + d, bottom - 20. + d, 20., Math.PI, (1. - 1.5 * tl.["f"]) * Math.PI, true)
        ctx.nodeArrow (220., bottom, 415., bottom + 75., -10., tl.["f"])
        ctx.nodeArrow (415., bottom + 75., 560., bottom - 20., -15., tl.["f"])
        ctx.nodeArrow (220., bottom, 560., bottom - 20., -20., tl.["f"])

        ctx.cloud (color "#fdf5") 390. (top + 20.) 260. 140. tl.["catOp"]
        ctx.node ("", 220., top, LeftAbove, tl.["catOp"], 55)
        ctx.arcArrow (220. - d, top - d, 20., 0.5 * Math.PI, (0.5 + 1.5 * tl.["catOp_f"]) * Math.PI)
        ctx.node ("", 415., top + 75., CenterUnder, tl.["catOp"], 55)
        ctx.arcArrow (415. + d, top + 75. + d, 20., Math.PI, (1. - 1.5 * tl.["catOp_f"]) * Math.PI, true)
        ctx.node ("", 560., top - 20., RightAbove, tl.["catOp"], 55)
        ctx.arcArrow (560. + d, top - 20. + d, 20., 1.5 * Math.PI, (1.5 + 1.5 * tl.["catOp_f"]) * Math.PI)
        ctx.nodeArrow (415., top + 75., 220., top, 10., tl.["catOp_f"])
        ctx.nodeArrow (560., top - 20., 415., top + 75., 15., tl.["catOp_f"])
        ctx.nodeArrow (560., top - 20., 220., top, 20., tl.["catOp_f"])
    )
}

let coproduct = scene "coproduct" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
        500 => fadeIn 750 EaseOutQuad "f"
    })
    run (animation {
        0 => fadeIn 750 Linear "cp"
        500 => fadeIn 750 EaseOutQuad "cp_f"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]
        let top = ctx.height * (1./3.) - 50.
        let bottom = ctx.height * (2./3.) + 50.
        ctx.style <- color "#fff"
        ctx.nodeSize <- 20.

        ctx.lineWidth <- 1.
        ctx.font <- serifFont 65
        ctx.drawText ("Product", 100., bottom - 120., tl.["init"])
        ctx.drawText ("Coproduct", 100., top - 120., tl.["cp"])

        ctx.lineWidth <- 3.

        ctx.node ("", 250., bottom, Left, tl.["init"])
        ctx.node ("", 500., bottom - 120., Right, tl.["init"])
        ctx.nodeArrow (250., bottom, 500., bottom - 120., -20., tl.["f"])
        ctx.node ("", 500., bottom + 120., Right, tl.["init"])
        ctx.nodeArrow (250., bottom, 500., bottom + 120., 20., tl.["f"])

        ctx.node ("", 250., top, Left, tl.["cp"])
        ctx.node ("", 500., top - 120., Right, tl.["cp"])
        ctx.nodeArrow (500., top - 120., 250., top, 20., tl.["cp_f"])
        ctx.node ("", 500., top + 120., Right, tl.["cp"])
        ctx.nodeArrow (500., top + 120., 250., top, -20., tl.["cp_f"])
    )
}

let comonad = scene "comonad" {
    enter (animation {
        0 => fadeIn 750 Linear "catK"
        1000 => fadeIn 750 Linear "catK_fl"
    })
    leave (animation {
        0 => fadeOut 500 Linear "opacity"
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.globalAlpha <- tl.["opacity"]
        let d = sqrt (30.**2. / 2.)
        let middle = ctx.height * (1./2.)

        ctx.setStyle (color "#fff")
        ctx.nodeSize <- 15.
        ctx.lineWidth <- 3.
        ctx.textBaseline <- "top"
        
        ctx.save ()
        ctx.font <- serifFont 65
        ctx.lineWidth <- 1.
        ctx.textBaseline <- "bottom"
        ctx.drawText ("co-Kleisli", 100., middle - 160., tl.["catK"])
        ctx.restore ()

        ctx.translate (-620., 0.)
        ctx.cloud (color "#efe5") 1020. middle 250. 135. (tl.["catK"])
        ctx.node ("a", 895., middle - 20., LeftAbove, tl.["catK"], 55)
        ctx.node ("b", 1090., middle + 30., CenterUnder, tl.["catK"], 55)
        ctx.nodeArrow (1090., middle + 30., 895., middle - 20., 40., tl.["catK"])
        ctx.arcArrow (895. - d, middle - 20. + d, 20., 1.5 * Math.PI, (1.5 - 1.5 * tl.["catK"]) * Math.PI, true)
        ctx.arcArrow (1090. + d, middle + 30. - d, 20., Math.PI, (1. + 1.5 * tl.["catK"]) * Math.PI)
        ctx.save ()
        ctx.lineWidth <- 1.
        ctx.lineHeight <- 1.
        ctx.font <- serifFont 40
        ctx.textBaseline <- "bottom"
        ctx.drawText ("Fb \u2192 a", 930., middle - 20., tl.["catK_fl"])
        ctx.drawText (" Fb", 1090. + 2.5 * d, middle + 30. - d, tl.["catK_fl"])
        ctx.drawText ("\u2192 b", 1090. + 2.5 * d, middle + 30. - d + ctx.actualLineHeight, tl.["catK_fl"])
        ctx.textBaseline <- "top"
        ctx.drawText ("Fa \u2192 a", 840., middle - 20. + 2.5 * d, tl.["catK_fl"])
        ctx.restore ()
    )
}

let scenes =
    [ monad; natTransf; product; homset; oppositeCat; coproduct; comonad ]
