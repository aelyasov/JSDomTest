function modify_region(mpressed, iheight, iwidth, idata, reach, centerx, centery) {
    if (mpressed) {
        for (var y = iheight - 1; y >= 0; y--) {
            for (var x = 0; x < iwidth; x++) {
                var pos = 4 * (iwidth * y + x);
                var a = x - centerx;
                var b = y - centery;
                var v = 1 - (a * a + b * b) / (reach * reach);
                if (v < 0) {
                    v = 0;
                }
                v = v * v;
                if (y >= 1) {
                    idata[pos + 0] += v * (idata[pos + 0 - iwidth * 4] - idata[pos + 0]);
                    idata[pos + 1] += v * (idata[pos + 1 - iwidth * 4] - idata[pos + 1]);
                    idata[pos + 2] += v * (idata[pos + 2 - iwidth * 4] - idata[pos + 2]);
                }
            }
        }
    } else {
        // This part is more optimized for fast execution.
        var rr = 1 / (reach * reach);
        var pos = 0;
        var w = iwidth;
        var t = 0;
        var v = 0;
        var a = 0;
        var vv = 0;
        for (var y = 0; y < iheight; y++) {
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

/*
 * Types:
 * mpressed: boolean  
 * iheight: int 
 * iwidth: int 
 * idata: [int] 
 * reach: int 
 * i 
 * centerx: int 
 * centery: int
 */