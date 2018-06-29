function test_attractor(a, b, c, d, e, f, g, h, epsilon, threshold, iterations) {
    var i, j, x = y = 0,
        minx = miny = 9999999999, 
        maxx = maxy = 0,
        seeds = [],
        px, py, tmpX, tmpY, unique, dx, dy;
    for (i = 0; i < iterations; i++) {
        px = Math.round(150 + x * 50);
        py = Math.round(150 + y * 50);

        tmpX = a * Math.sin(b * y) + c * Math.cos(d * x);
        tmpY = e * Math.sin(f * x) + g * Math.cos(h * y);

        x = tmpX;
        y = tmpY;

        if (seeds.length == 0) seeds.push([px, py]);
        else {
            unique = 1;
            for (j = 0; j < seeds.length; j++) {
                dx = Math.abs(seeds[j][0] - px);
                dy = Math.abs(seeds[j][1] - py);
                if (dx < epsilon && dy < epsilon) unique = 0;
            }
            if (unique) {
                seeds.push([px, py]);
                if (px < minx) minx = px;
                else if (px > maxx) maxx = px;
                if (py < miny) miny = py;
                else if (py > maxy) maxy = py;
            }
        }
    }
    return {
        'ok': seeds.length > threshold,
        'minx': minx,
        'maxx': maxx,
        'miny': miny,
        'maxy': maxy
    };
}

/*
 * Types:
 * a: float
 * b: float
 * c: float
 * d: float
 * e: float
 * f: float
 * g: float
 * h: float
 * epsilon: int
 * threshold: int
 * iterations: int
 */
