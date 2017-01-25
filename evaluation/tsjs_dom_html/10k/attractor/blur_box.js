function blur_box(ids, idd, bx, by) {
    var src = $(ids);
    var sc = cc(src);
    var width = src.width;
    var height = src.height;
    var dst = $(idd);
    var dc = cc(dst);
    var sd = sc.getImageData(0, 0, width, height);
    var dd = dc.getImageData(0, 0, width, height);

    if (bx < 0 || by < 0) dc.drawImage(src, 0, 0);

    if (!TDATA) TDATA = new Array(width * height * 4);
    precompute(sd, TDATA, width, height);

    var x, y, tot, n = i = 0,
        c = 3,
        mul = 1 / ((bx * 2 + 1) * (by * 2 + 1));
    for (y = 0; y < height; y++) {
        for (x = 0; x < width; x++) {
            tot = pxl(TDATA, width, height, x + bx, y + by, c) +
                pxl(TDATA, width, height, x - bx, y - by, c) -
                pxl(TDATA, width, height, x - bx, y + by, c) -
                pxl(TDATA, width, height, x + bx, y - by, c);

            n = i * 4;
            dd.data[n] = 0xff;
            dd.data[n + 1] = 0xff;
            dd.data[n + 2] = 0xff;
            dd.data[n + 3] = tot * mul;

            i++;
        }
    }
    dc.putImageData(dd, 0, 0);
}

var TDATA = 0;

function precompute(src, dst, w, h) {
    var x, y, tot = i = 0;
    for (y = 0; y < h; y++)
        for (x = 0; x < w; x++) {
            tot = src.data[4 * i + 3];
            if (x > 0) tot += dst[(i - 1) * 4 + 3];
            if (y > 0) tot += dst[(i - w) * 4 + 3];
            if (x > 0 && y > 0) tot -= dst[(i - w - 1) * 4 + 3];
            dst[4 * i + 3] = tot;
            i++;
        }
}

function clamp(x, l, h) {
    return x < l ? l : x > h ? h : x
}

function pxl(p, w, h, x, y, c) {
    x = clamp(x, 0, w - 1);
    y = clamp(y, 0, y - 1);
    return p[4 * (x + y * w) + c]
}

function $(id) {
    return document.getElementById(id)
}

function cc(e) {
    return e.getContext("2d")
}