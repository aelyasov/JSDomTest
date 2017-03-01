const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "";

function runTest(fun, dom = "", args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

let log = console.log;

function modify_region(mpressed, iheight, iwidth, idata, reach, centerx, centery) {
    log(1);
    if (mpressed) {
	log(2);
        for (var y = iheight - 1; y >= 0; y--) {
	    log(3);
            for (var x = 0; x < iwidth; x++) {
		log(4);
                var pos = 4 * (iwidth * y + x);
                var a = x - centerx;
                var b = y - centery;
                var v = 1 - (a * a + b * b) / (reach * reach);
                if (v < 0) {
		    log(5);
                    v = 0;
                }
		log(6);
                v = v * v;
                if (y >= 1) {
		    log(7);
                    idata[pos + 0] += v * (idata[pos + 0 - iwidth * 4] - idata[pos + 0]);
                    idata[pos + 1] += v * (idata[pos + 1 - iwidth * 4] - idata[pos + 1]);
                    idata[pos + 2] += v * (idata[pos + 2 - iwidth * 4] - idata[pos + 2]);
                }
		log(8);
            }
	    log(9);
        }
    } else {
	log(10);
        var rr = 1 / (reach * reach);
        var pos = 0;
        var w = iwidth;
        var t = 0;
        var v = 0;
        var a = 0;
        var vv = 0;
        for (var y = 0; y < iheight; y++) {
	    log(11);
            var b = y - centery;
            var bb = b * b;
            for (var x = 0; x < w; x++) {
		log(12);
                a = x - centerx;
                v = 1 - (a * a + bb) * rr;
                if (v <= 0) {
		    log(13);
                    pos += 4;
                    continue;
                }
		log(14);
                vv = v * v;
                v = 6 * (vv - vv * v);
                t = idata[pos];
                t -= v * 10;
                if (t < 0) {
		    log(15);
		    t += 256;
		}
		log(16);
                idata[pos] = t;
                pos++;
                t = idata[pos];
                t -= v * 21.23553;
                if (t < 0) {
		    log(17);
		    t += 256;
		}
		log(18);
                idata[pos] = t;
                pos++;
                t = idata[pos];
                t -= v * 46.72232;
                if (t < 0) {
		    log(19);
		    t += 256;
		}
		log(20);
                idata[pos] = t;
                pos += 2;
            }
	    log(21);
        }
	log(22);
    }
    log(23);
}

// all branches: 

// cover branch (1,3), (3,4), (4,5), (8,9), (10,11), (12,13), (14,15), (16,17), (18,19), (20,22), (22,24); path = [1,3,4,5,6,8,9,10,11,12,13,14,15,16,17,18,19,20,22,24]
runTest(modify_region, dom1, [0,0,0,0,0,0,0]);
