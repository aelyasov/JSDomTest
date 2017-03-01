const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='PING'></div></body></html>";

function runTest(fun, dom = "", args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

let log = console.log;

function goPing(pingH, pingV, iteration) {
    var PING = document.getElementById("PING");
    log(1);
    if (iteration == 1) {
	log(2);
        PING.src = "img/ping1.png";
        moveObjTo(PING, pingH, pingV);
        window.setTimeout("goPing(" + pingH + "," + pingV + ",2);", 80);
    } else if (iteration == 2) {
	log(3);
        PING.src = "img/ping2.png";
        window.setTimeout("goPing(0,0,3);", 80);
    } else {
	log(4);
        moveObjTo(PING, -100, -100);
    }
    log(5);
}

function moveObjTo(obj, oleft, otop) {
    obj.style.left = oleft;
    obj.style.top = otop;
}

// all branches: (1,2), (1,3), (1,4)

// cover branch (1,4); path = [1,4,5]
runTest(goPing, dom1, [0,0,0]);

// cover branch (1,2); path = [1,2,5]
runTest(goPing, dom1, [0,0,1]);

// cover branch (1,2); path = [1,3,5]
runTest(goPing, dom1, [0,0,2]);
