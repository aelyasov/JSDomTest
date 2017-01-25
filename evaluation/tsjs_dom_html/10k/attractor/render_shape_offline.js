function render_shape_offline(id, iterations, size, x, y, a, b, c, d, e, f, g, h, zoom, dx, dy) {
    var i;
    var canvas = document.getElementById(id),
        ctx = canvas.getContext("2d"),
        width = canvas.width,
        height = canvas.height,
        cx = dx + width / 2,
        cy = dy + height / 2;

    ctx.clearRect(0, 0, width, height);

    var l = width * height;
    if (!DATA) {
        DATA = new Array(l)
    }
    for (i = 0; i < l; ++i) DATA[i] = 0;

    var tmpX, tmpY, px, py;
    for (i = 0; i < iterations; ++i) {
        px = Math.round(cx + x * 50 * zoom);
        py = Math.round(cy + y * 50 * zoom);
        if (px >= 0 && px < width && py >= 0 && py < height) DATA[px + py * width]++;

        tmpX = a * Math.sin(b * y) + c * Math.cos(d * x);
        tmpY = e * Math.sin(f * x) + g * Math.cos(h * y);

        x = tmpX;
        y = tmpY;
    }

    var datamax = DATA[0];
    for (i = 1; i < l; ++i)
        if (DATA[i] > datamax) datamax = DATA[i];

    var q = ctx.getImageData(0, 0, width, height);
    for (i = 0; i < l; ++i) {
        if (DATA[i] > 0) {
            n = i * 4;
            q.data[n] = 0xff;
            q.data[n + 1] = 0xff;
            q.data[n + 2] = 0xff;
            q.data[n + 3] = clamp(255 * (0.025 + 4 * DATA[i] / datamax), 0, 0xff);
        }
    }
    ctx.putImageData(q, 0, 0);
}

var DATA = 0;


function clamp(x, l, h) {
    return x < l ? l : x > h ? h : x
}

/*
 * Types:
 * id: string
 * iterations: int
 * size: int
 * x: int
 * y: int
 * a: float
 * b: float
 * c: float
 * d: float
 * e: float
 * f: float
 * g: float
 * h: float
 * zoom: float
 * dx: float
 * dy: float
 */