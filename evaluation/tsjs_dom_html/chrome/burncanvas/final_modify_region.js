/*t bool:int:int:[int]:int:int:int */
function test(mpressed, iheight, iwidth, idata, reach, centerx, centery) {
    if (mpressed) {
       var y1 = iheight - 1; 
        for (var y = 0; y <= y1; y++) {
            y1 = y1 - y;
            for (var x = 0; x < iwidth; x++) {
                var pos = 4 * (iwidth * y1 + x);
                var a = x - centerx;
                var b = y1 - centery;
                var v = 1 - (a * a + b * b) / (reach * reach);
                 console.log("v: ", v);
               if (v < 0) {
                     v = 0;
                }
                v = v * v;
              console.log("y1: ", y1);
                if (y1 >= 1) {
                    idata[pos + 0] += v * (idata[pos + 0 - iwidth * 4] - idata[pos + 0]);
                    idata[pos + 1] += v * (idata[pos + 1 - iwidth * 4] - idata[pos + 1]);
                    idata[pos + 2] += v * (idata[pos + 2 - iwidth * 4] - idata[pos + 2]);
                }
            }
        }
    } else {
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
              console.log("v: ", v);
                if (v <= 0) {
                    pos += 4;
                    continue;
                }
                vv = v * v;
                v = 6 * (vv - vv * v);
                t = idata[pos];
                t -= v * 10;
              console.log("t: ", t);
                if (t < 0) {
		    t += 256;
		}
                idata[pos] = t;
                pos++;
                t = idata[pos];
                t -= v * 21.23553;
              console.log("t: ", t);
                if (t < 0) {
		    t += 256;
		}
                idata[pos] = t;
                pos++;
                t = idata[pos];
                t -= v * 46.72232;
              console.log("t: ", t);
                if (t < 0) {
		    t += 256;
		}
                idata[pos] = t;
                pos += 2;
            }
        }
    }
}