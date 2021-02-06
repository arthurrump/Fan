module CTP.V5

open CTP.Shared
open System
open Browser.Types
open Animation.Animation
open Canvas

let monad = scene "monad" {
    enter (animation {
        0 => fadeIn 750 Linear "init"
        250 => fadeIn 750 EaseOutQuad "init_funcs"
        500 => fadeIn 750 EaseOutQuad "functor"
        1000 => fadeIn 750 Linear "catF"
        1250 => fadeIn 750 EaseOutQuad "catF_funcs"
    })
    run (animation {
        0 => fadeIn 750 Linear "subcat"
        1000 => fadeIn 750 EaseOutQuad "catK"
        2000 => fadeIn 750 Linear "catK_fl"
        3000 => fadeIn 750 EaseOutQuad "monad"
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

let scenes =
    [ monad ]
