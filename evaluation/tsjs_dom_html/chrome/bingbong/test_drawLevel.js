const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='LEVEL'></div><div id='LEVEL1'></div><div id='LEVEL2'></div></body></html>";

function runTest(fun, dom = "", args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

let log = console.log;

function drawLevel(iteration, theLevel, myCenterH, myCenterV) {
    var LEVEL = document.getElementById("LEVEL");
    var LEVEL1 = document.getElementById("LEVEL1");
    var LEVEL2 = document.getElementById("LEVEL2");
    log(1);
    if (iteration == 1) {
	log(2);
        var stringLevel = theLevel + '';
        var i = 0;
        for (i = 1; i <= 2; i++) {
	    log(3);
            if (stringLevel.length < 2) {
		log(4);
                stringLevel = '0' + stringLevel;
            }
	    log(5);
        }
	log(6);
        for (i = 1; i <= 2; i++) {
	    log(7);
            document.getElementById('LEVEL' + i).src = "img/num" + stringLevel.charAt(i - 1) + ".png";
        }
	log(8);
        moveObjTo(LEVEL, myCenterH - 75, myCenterV + 30);
        moveObjTo(LEVEL1, myCenterH + 25, myCenterV + 25);
        moveObjTo(LEVEL2, myCenterH + 50, myCenterV + 25);
        window.setTimeout("drawLevel(2);", 1400);
    } else {
	log(9);
        moveObjTo(LEVEL, -100, -100);
        moveObjTo(LEVEL1, -100, -100);
        moveObjTo(LEVEL2, -100, -100);
    }
    log(10);
}

function moveObjTo(obj, oleft, otop) {
    obj.style.left = oleft;
    obj.style.top = otop;
}

// all branches: (1,2), (2,3), (3,4), (3,5), (2,6), (6,7), (6,8), (1,9)

// cover branch (1,9); path = [1,9,10]
runTest(drawLevel, dom1, [0,0,0,0]);

// cover branch (1,2), (3,4), (3,5), (6,7); path = [1,2,3,4,5,3,5,6,7,7,8,10]
runTest(drawLevel, dom1, [1,0,0,0]);
