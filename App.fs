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
            "rectOffset" => 0
        }
        2000 => vars {
            "rectSize" => (100, EaseOutBack)
            "rectOffset" => 20
        }
    })
    run (timeline' (Normal, Repeat 2) {
        0 => vars {
            "rotation" => 0.
        }
        1500 => vars {
            "rotation" => 2. * Math.PI
        }
    })
    leave (timeline {
        0 => vars {
            "opacity" => 1.
        }
        1000 => vars {
            "opacity" => 0.
            "rectSize" => (0, EaseInExpo)
        }
    })
    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.clearRect (0., 0., cnv.width, cnv.height)
        let offset = tl.["rectOffset"]
        let size = tl.["rectSize"]
        let center = offset + size / 2.
        let topLeft = -size / 2.
        ctx.translate (center, center)
        ctx.rotate (tl.["rotation"])
        ctx.beginPath ()
        ctx.rect (topLeft, topLeft, size, size)
        ctx.fillStyle <- rgba (255, 20, 100, tl.["opacity"])
        ctx.fill ()
    )
}

let grid size style (ctx : CanvasRenderingContext2D) =
    ctx.save ()
    ctx.lineWidth <- 1.
    ctx.strokeStyle <- style
    ctx.fillStyle <- style
    ctx.font <- sprintf "%ipx sans-serif" (int size / 4)
    ctx.textBaseline <- "top"
    for i in 0. .. size .. ctx.width do
        ctx.beginPath ()
        ctx.moveTo (i, 0.)
        ctx.lineTo (i, ctx.height)
        ctx.stroke ()
        ctx.fillText (string i, i + (size / 10.), size / 10.)
    ctx.textBaseline <- "alphabetic"
    for i in 0. .. size .. ctx.height do
        ctx.beginPath ()
        ctx.moveTo (0., i)
        ctx.lineTo (ctx.width, i)
        ctx.stroke ()
        ctx.fillText (string i, size / 10., i - (size / 10.))
    ctx.restore ()

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
    } |> Timeline.delay 1000.)

    render (fun (ctx : CanvasRenderingContext2D) tl ->
        ctx.background (rgb (0, 0, 0))
        ctx.save ()
        ctx.translate (10., 10.)

        ctx |> grid 50. (rgba (150, 150, 150, tl.["gridOpacity"]))

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

let helloWorld = scene {
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
        ctx.font <- CTP.serifFont 200
        ctx.setStyle (color "#fff")
        ctx.textBaseline <- "middle"

        let x = (ctx.width - ctx.measureText("Hello, world!").width) / 2.
        ctx.drawText ("Hello, world!", x, ctx.height / 2., tl.["progress"], 0.5)
    )
}

let haskell = scene {
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
            [ [ (CTP.CodeColors.funcDecl, "id"); (CTP.CodeColors.operator, " :: "); (CTP.CodeColors.var, "a"); (CTP.CodeColors.operator, " -> "); (CTP.CodeColors.var, "a") ]
              [ (CTP.CodeColors.text, "id "); (CTP.CodeColors.text, "x"); (CTP.CodeColors.text, " = "); (CTP.CodeColors.text, "x") ] ]
        
        ctx.font <- CTP.codeFont 200
        ctx.setStyle (color "#fff")
        ctx.textBaseline <- "top"
        ctx.drawLongText (text, 100., (ctx.height - float text.Length * ctx.actualLineHeight * 1.2) / 2., tl.["progress"])

        ctx.restore ()
    )
}

// let ind =
//     match Int32.TryParse(window.location.hash.TrimStart('#')) with
//     | true, i when i >= 0 -> i
//     | true, i -> scenes.Length + i
//     | false, _ -> 0

// window.onhashchange <- fun _ -> window.location.reload (true)

CTP.scenes
|> List.map (Scene.withRender (fun (ctx : CanvasRenderingContext2D) tl t render -> 
    ctx.clear ()
    ctx.save ()
    // ctx |> grid 100. (color "#eee")
    render ctx tl t
    ctx.restore ()
))
|> List.head
|> Render.runRender
//|> Preview.runPreview ctx "app"