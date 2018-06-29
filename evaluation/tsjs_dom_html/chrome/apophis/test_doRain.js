const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='test'/></body></html>";

function runTest(fun, dom = "", args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

let log = console.log;

//string,int,int,int,int,int,int
function doRain(id, locX, locY, speed, tankLive, myHeight, myWidth) {
    var obj = document.getElementById(id);

    log(1);
    if (tankLive == 1) {
	log(2);
        if (locX < -70 || locY > myHeight) {
	    log(3);
            locX = (Math.random() * (myWidth + 300)) + 100;
            locY = -150;
        } else {
	    log(4);
            locX = locX - (0.76 * speed);
            locY = locY + (1.71 * speed);
        }
	log(5);
        moveObjTo(obj, locX, locY);
    }
    log(6);
    window.setTimeout("doRain(" + obj.id + "," + locX + "," + locY + "," + speed + ");", 25);
}

//Simple move function
function moveObjTo(obj, oleft, otop) {
    obj.style.left = oleft;
    obj.style.top = otop;
}

// all branches: (1,2), (1,6), (2,3), (2,4)

// cover branch (1,2), (2,4); path = [1,2,4,5,6]
runTest(doRain, dom1, ["test",10,10,10,1,10,10]);

// cover branch (1,6); path = [1,6]
runTest(doRain, dom1, ["test",10,10,10,2,10,10]);

// cover branch (1,2), (2,3); path = [1,2,3,5,6]
runTest(doRain, dom1, ["test",-71,10,10,1,10,10]);
