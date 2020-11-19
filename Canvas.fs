module Canvas

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
