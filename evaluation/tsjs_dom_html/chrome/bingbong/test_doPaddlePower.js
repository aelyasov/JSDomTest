const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='PADDLEPOWERBAR'></div><div id='PADDLEPOWEROUTLINE'></div></body></html>";

function runTest(fun, dom = "", args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

let log = console.log;

function doPaddlePower(eventY, myHeight) {
    var PADDLEPOWERBAR = document.getElementById("PADDLEPOWERBAR");
    var PADDLEPOWEROUTLINE = document.getElementById("PADDLEPOWEROUTLINE");
    var paddlePower = eventY - 100;
    log(1);
    if (paddlePower < 1) {
	log(2);
        paddlePower = 0;
    }
    log(3);
    paddlePower = (paddlePower / (myHeight - 200)) * 100;
    if (paddlePower > 100) {
	log(4);
        paddlePower = 100;
    }
    log(5);
    paddlePower = 100 - paddlePower;
    PADDLEPOWERBAR.height = (153 * paddlePower / 100);
    moveObjTo(PADDLEPOWEROUTLINE, 20, myHeight - 200);
    moveObjTo(PADDLEPOWERBAR, 26, myHeight - 25 - PADDLEPOWERBAR.height);
}

function moveObjTo(obj, oleft, otop) {
    obj.style.left = oleft;
    obj.style.top = otop;
}

// all branches: (1,2), (1,3), (3,4), (3,5)

// cover branch (1,3), (3,5); path = [1,3,5]
runTest(doPaddlePower, dom1, [102,0]);

// cover branch (1,2), (3,5); path = [1,2,3,5]
runTest(doPaddlePower, dom1, [0,0]);

// cover branch (1,3), (4,5); path = [1,3,4,5]
runTest(doPaddlePower, dom1, [104,201]);
