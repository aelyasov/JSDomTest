function modify_region(i, centerx, centery) {
    if (m.pressed) {
        for (var y = i.height - 1; y >= 0; y--) {
            for (var x = 0; x < i.width; x++) {
                var pos = 4 * (i.width * y + x);
                var a = x - centerx;
                var b = y - centery;
                var v = 1 - (a * a + b * b) / (reach * reach);
                if (v < 0) {
                    v = 0;
                }
                v = v * v;
                if (y >= 1) {
                    i.data[pos + 0] += v * (i.data[pos + 0 - i.width * 4] - i.data[pos + 0]);
                    i.data[pos + 1] += v * (i.data[pos + 1 - i.width * 4] - i.data[pos + 1]);
                    i.data[pos + 2] += v * (i.data[pos + 2 - i.width * 4] - i.data[pos + 2]);
                }
            }
        }
    } else {
        // This part is more optimized for fast execution.
        var rr = 1 / (reach * reach);
        var pos = 0;
        var w = i.width;
        var t = 0;
        var v = 0;
        var a = 0;
        var vv = 0;
        var idata = i.data;
        for (var y = 0; y < i.height; y++) {
            var b = y - centery;
            var bb = b * b;
            for (var x = 0; x < w; x++) {
                a = x - centerx;
                v = 1 - (a * a + bb) * rr;
                if (v <= 0) {
                    pos += 4;
                    continue;
                }
                vv = v * v;
                v = 6 * (vv - vv * v);
                t = idata[pos];
                t -= v * 10;
                if (t < 0) t += 256;
                idata[pos] = t;
                pos++;
                t = idata[pos];
                t -= v * 21.23553;
                if (t < 0) t += 256;
                idata[pos] = t;
                pos++;
                t = idata[pos];
                t -= v * 46.72232;
                if (t < 0) t += 256;
                idata[pos] = t;
                pos += 2;
            }
        }
    }
}

var reach;