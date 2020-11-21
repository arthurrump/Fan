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

        ctx.fillStyle <- U3.Case1 "grey"
        ctx.fill ()

        ctx.restore ()

let cnv = document.getElementById "animation" :?> HTMLCanvasElement
let ctx = cnv.getContext_2d ()

// let render t =
//     ctx.fillStyle <- U3.Case1 "rgba(10, 100, 200, 1)"
//     ctx.clearRect (0., 0., cnv.width, cnv.height)

//     ctx.save ()
//     ctx.translate (250., 200.)
//     ctx |> Sun.draw {
//         Sun.Radius = t |> animation 100 { 
//             keys [ key { value 102 } ]
//             duration 600
//             direction Alternate
//             loop Infinite 
//         }
//         Sun.RayLength = t |> animation 50 { 
//             keys [ key { value 100 } ]
//             duration 1000
//             direction Alternate
//             loop Infinite
//         }
//         Sun.RayRotation = t |> animation 0 {
//             keys [ key { value (2. * Math.PI) } ]
//             duration 11500
//             loop Infinite
//         }
//     }
//     ctx.restore ()

//     ctx.save ()
//     let x = t |> animation -200 {
//         keys [ key { value (cnv.width + 100.) } ]
//         duration 4300
//         loop Infinite
//     }
//     ctx.translate (x, 350.)
//     ctx |> Cloud.draw { Cloud.Size = 2. }
//     ctx.restore ()

// runAnimation render

let tl = timeline' (Alternate, Repeat 3) {
    100 => vars {
        "rectSize" => 0
    }
    1000 => vars {
        "rectSize" => 10
        "rectOffset" => 0.
    }
    2000 => vars {
        "rectSize" => key { value 100; easing Linear }
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

runAnimation scene