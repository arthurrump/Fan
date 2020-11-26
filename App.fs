module App

open Animation
open Canvas
open System
open Fable.Core
open Browser.Dom
open Browser.Types

let cnv = document.getElementById "animation" :?> HTMLCanvasElement
let ctx = cnv.getContext_2d ()

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

type AnimationVars = SunRadius | SunRayLength | SunRayRotation | CloudX

let sunshine = scene {
    run (animation {
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
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.fillStyle <- U3.Case1 "rgba(10, 100, 200, 1)"
        ctx.clearRect (0., 0., cnv.width, cnv.height)

        ctx.save ()
        ctx.translate (250., 200.)
        ctx |> Sun.draw {
            Sun.Radius = tl.[SunRadius]
            Sun.RayLength = tl.[SunRayLength]
            Sun.RayRotation = tl.[SunRayRotation]
        }
        ctx.restore ()

        ctx.save ()
        ctx.translate (tl.[CloudX], 350.)
        ctx |> Cloud.draw { Cloud.Size = 2. }
        ctx.restore ()
    )
}

let square = scene {
    enter (timeline' (Alternate, Repeat 3) {
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
    })
    // TODO: run should not be needed here
    run (timeline { 0 => vars { "rectSize" => 100; "rectOffset" => 20. } })
    leave (timeline {
        0 => vars {
            // TODO: get the starting value from the preceding timeline
            "rectSize" => 100
        }
        1000 => vars {
            "rectSize" => 0
        }
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.clearRect (0., 0., cnv.width, cnv.height)
        ctx.beginPath ()
        let offset = tl.["rectOffset"]
        let size = tl.["rectSize"]
        ctx.rect (offset, offset, size, size)
        ctx.fillStyle <- rgb (255, 20, 100)
        ctx.fill ()
    )
}

let arrow = scene {
    enter (timeline {
        0 => vars {
            "gridOpacity" => 0
        }
        500 => vars {
            "gridOpacity" => 1
        }
    })

    run (timeline' (Normal, Infinite) {
        0 => vars {
            "arrowDraw" => 0
            // TODO: don't require values to be set in consequitive timelines
            "gridOpacity" => 1
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
    } |> Timeline.delay 1000)

    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.background (rgb (0, 0, 0))
        ctx.save ()
        ctx.translate (10., 10.)

        ctx.lineWidth <- 1.
        ctx.strokeStyle <- rgba (150, 150, 150, tl.["gridOpacity"])
        for i in 0. .. 50. .. ctx.width do
            ctx.beginPath ()
            ctx.moveTo (i, -10.)
            ctx.lineTo (i, ctx.height)
            ctx.stroke ()
        for i in 0. .. 50. .. ctx.height do
            ctx.beginPath ()
            ctx.moveTo (-10., i)
            ctx.lineTo (ctx.width, i)
            ctx.stroke ()

        ctx.lineWidth <- 3.
        ctx.strokeStyle <- rgb (255, 100, 50)
        ctx.fillStyle <- rgb (255, 0, 0)
        ctx.arrow (100., 150., 400., 150., 50., tl.["arrowDraw"], tl.["arrowErase"])

        ctx.beginPath ()
        ctx.curve (100., 200., 400., 200., 50., tl.["arrowDraw"], tl.["arrowErase"])
        ctx.stroke ()

        ctx.restore()
    )
}

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

let text = scene {
    run (timeline' (Alternate, Infinite) {
        1000 => vars {
            "progress" => 0
        }
        2000 => vars {
            "progress" => 1
        }
        3000 => []
    })
    render (fun (ctx : CanvasRenderingContext2D) tl t ->
        ctx.background (rgb (0, 0, 0))
        ctx.save ()

        let text = 
            [ [ (funcDecl, "id"); (operator, " :: "); (var, "a"); (operator, " -> "); (var, "a") ]
              [ (text, "id "); (text, "x"); (text, " = "); (text, "x") ] ]
        
        ctx.font <- codeFont 200
        ctx.setStyle (color "#fff")
        ctx.textBaseline <- "top"
        ctx.drawLongText (text, 100., (ctx.height - float text.Length * ctx.currentLineHeight * 1.2) / 2., tl.Function "progress", t)

        ctx.restore ()
    )
}

Scene.run text ctx