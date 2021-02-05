module CTP.V3

open CTP.Shared
open System
open Browser.Types
open Animation.Animation
open Canvas

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

        ctx.node ("a", 250., kleisli, LeftUnder, (stagger) 2 1 tl.["init"])
        ctx.node ("b", 600., kleisli, RightUnder, (stagger) 2 1 tl.["init"])
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

let scenes =
    [ kleisliWriter ]
