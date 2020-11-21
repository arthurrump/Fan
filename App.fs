module App

open Animation
open Canvas
open System
open Fable.Core
open Browser.Dom
open Browser.Types

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

let cnv = document.getElementById "animation" :?> HTMLCanvasElement
let ctx = cnv.getContext_2d ()

type AnimationVars = SunRadius | SunRayLength | SunRayRotation | CloudX

let render t =
    let tl = timeline' (Alternate, Infinite) {
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

    let tl2 = timeline' (Normal, Infinite) {
        0 => vars {
            SunRayRotation => 0
            CloudX => -200
        }
        3000 => vars {
            SunRayRotation => 0.5 * Math.PI
            CloudX => (cnv.width + 100., EaseOutSine)
        }
    }

    ctx.fillStyle <- U3.Case1 "rgba(10, 100, 200, 1)"
    ctx.clearRect (0., 0., cnv.width, cnv.height)

    ctx.save ()
    ctx.translate (250., 200.)
    let sun = {
        Sun.Radius = tl.[SunRadius] t
        Sun.RayLength = tl.[SunRayLength] t
        Sun.RayRotation = tl2.[SunRayRotation] t
    }
    printfn "Sun = %A" sun
    ctx |> Sun.draw sun
    ctx.restore ()

    ctx.save ()
    let x = tl2.[CloudX] t
    printfn "CloudX = %f" x
    ctx.translate (x, 350.)
    ctx |> Cloud.draw { Cloud.Size = 2. }
    ctx.restore ()

runAnimation render

let tl = timeline' (Alternate, Repeat 3) {
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
}

let scene =
    fun t ->
        ctx.clearRect (0., 0., cnv.width, cnv.height)
        ctx.beginPath ()
        let offset = tl.["rectOffset"] t
        let size = tl.["rectSize"] t
        ctx.rect (offset, offset, size, size)
        ctx.fillStyle <- rgb (255, 20, 100)
        ctx.fill ()

// runAnimation scene