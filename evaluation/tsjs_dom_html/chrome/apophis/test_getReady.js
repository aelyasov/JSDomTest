const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='TANK1'></div><div id='GETREADY'></div></body></html>";

let dom2 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body></body></html>";

let dom3 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='GETREADY'></div></body></html>";

function runTest(fun, dom = "", args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

let log = console.log;

function getReady(iteration, tankLive, meteorLive, tankH, myCenterH, myCenterV) {
    log(1);
    if (iteration == 1) {
	log(2);
        tankLive = 0;
        meteorLive = 0;
        tankH = -50;
        document.getElementById('TANK1').style.left = -50;
        document.getElementById('GETREADY').style.left = myCenterH - 97;
        document.getElementById('GETREADY').style.top = myCenterV - 16;
        window.setTimeout("getReady(2);", 1000);
    } else if (iteration == 2) {
	log(3);
	tankLive = 1;
        window.setTimeout("getReady(3);", 500);
    } else {
	log(4);
        document.getElementById('GETREADY').style.top = -500;
        meteorLive = 1;
    }
    log(5);
}

// all branches: (1,2), (1,3), (1,4)

// cover branch (1,2); path = [1,2,5]
runTest(getReady, dom1, [1,0,0,0,0,0]);

// cover branch (1,3); path = [1,3,5]
runTest(getReady, dom2, [2,0,0,0,0,0]);

// cover branch (1,4); path = [1,4,5]
runTest(getReady, dom3, [3,0,0,0,0,0]);
