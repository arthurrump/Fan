module Perlin

// https://en.wikipedia.org/wiki/Perlin_noise

let private interpolate a0 a1 w =
    (a1 - a0) * w + a0

let private randomGradient ix iy =
    let r = 2920. * sin(ix * 21942. + iy * 171324. + 8912.) * cos(ix * 23157. * iy * 217832. + 9758.)
    (cos r, sin r)

let private dotGridGradient ix iy x y =
    let (gx, gy) = randomGradient ix iy
    (x - ix) * gx + (y - iy) * gy

let noise x y =
    let x0, x1 = floor x, ceil x
    let y0, y1 = floor y, ceil y

    let sx = x - x0
    let sy = y - y0

    let n0 = dotGridGradient x0 y0 x y
    let n1 = dotGridGradient x1 y0 x y
    let ix0 = interpolate n0 n1 sx

    let n0 = dotGridGradient x0 y1 x y
    let n1 = dotGridGradient x1 y1 x y
    let ix1 = interpolate n0 n1 sx

    interpolate ix0 ix1 sy