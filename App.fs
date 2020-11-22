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

let tl = animation {
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
}

let render t =
    ctx.fillStyle <- U3.Case1 "rgba(10, 100, 200, 1)"
    ctx.clearRect (0., 0., cnv.width, cnv.height)

    ctx.save ()
    ctx.translate (250., 200.)
    ctx |> Sun.draw {
        Sun.Radius = tl.[SunRadius] t
        Sun.RayLength = tl.[SunRayLength] t
        Sun.RayRotation = tl.[SunRayRotation] t
    }
    ctx.restore ()

    ctx.save ()
    ctx.translate (tl.[CloudX] t, 350.)
    ctx |> Cloud.draw { Cloud.Size = 2. }
    ctx.restore ()

// runAnimation render

let tl2 = animationSingle <| timeline' (Alternate, Repeat 3) {
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
        let offset = tl2.["rectOffset"] t
        let size = tl2.["rectSize"] t
        ctx.rect (offset, offset, size, size)
        ctx.fillStyle <- rgb (255, 20, 100)
        ctx.fill ()

// runAnimation scene

let gtl = animationSingle <| timeline' (Normal, Infinite) {
    0 => vars {
        "arrowDraw" => 0
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
}

let grid t =
    ctx.background ()
    ctx.save ()

    ctx.lineWidth <- 1.
    ctx.strokeStyle <- rgba (0, 0, 0, 0.5)
    for i in 0. .. 50. .. ctx.width do
        ctx.beginPath ()
        ctx.moveTo (i, 0.)
        ctx.lineTo (i, ctx.height)
        ctx.stroke ()
    for i in 0. .. 50. .. ctx.height do
        ctx.beginPath ()
        ctx.moveTo (0., i)
        ctx.lineTo (ctx.width, i)
        ctx.stroke ()

    ctx.lineWidth <- 3.
    ctx.strokeStyle <- rgb (255, 100, 50)
    ctx.fillStyle <- rgb (255, 0, 0)
    ctx.arrow (100., 150., 400., 150., 50., gtl.["arrowDraw"] t, gtl.["arrowErase"] t)

    ctx.beginPath ()
    ctx.curve (100., 200., 400., 200., 50., gtl.["arrowDraw"] t, gtl.["arrowErase"] t)
    ctx.stroke ()

    ctx.restore()

runAnimation grid