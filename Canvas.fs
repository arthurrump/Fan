module Canvas

open Fable.Core
open Browser.Types

type CanvasRenderingContext2D with
    member ctx.ellipse (x, y, radiusX, ?radiusY, ?rotation, ?startAngle, ?endAngle) =
        let radiusY = defaultArg radiusY radiusX
        let rotation = defaultArg rotation 0.
        let startAngle = defaultArg startAngle 0.
        let endAngle = defaultArg endAngle (2. *  System.Math.PI)
        ctx.ellipse (x, y, radiusX, radiusY, rotation, startAngle, endAngle, false)
    member ctx.square (x, y, size) =
        ctx.rect (x, y, size, size)

let inline rgb (r, g, b) = U3.Case1 (sprintf "rgb(%f, %f, %f)" (float r) (float g) (float b))
let inline rgba (r, g, b, a) = U3.Case1 (sprintf "rgba(%f, %f, %f, %f)" (float r) (float g) (float b) a)
let inline color (str : string) = U3.Case1 str