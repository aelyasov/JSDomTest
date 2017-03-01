const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "</body></html>";

let dom2 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='BRICK0'></div></body></html>";


function runTest(fun, dom = "", args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

let log = console.log;

function brickJiggler(whichOne, iteration, speed, brickStat, brickH, brickV) {
    log(1);
    if (brickStat[whichOne] == 1) {
	log(2);
        iteration++;
        var tempTop = brickV[whichOne] + (Math.sin((iteration) / 6) * 2);
        document.getElementById('BRICK' + whichOne).style.top = tempTop;
        var tempLeft = brickH[whichOne] + (Math.cos((iteration) / 7) * 2);
        document.getElementById('BRICK' + whichOne).style.left = tempLeft;
    }
    log(3);
    window.setTimeout("brickJiggler(" + whichOne + "," + iteration + "," + speed + ");", speed);
}

// all branches: (1,2), (1,3)

// cover branch (1,3); path = [1,3]
runTest(brickJiggler, dom1, [1,1,10,[],[],[]]);

// cover branch (1,2); path = [1,2,3]
runTest(brickJiggler, dom2, [0,1,10,[1],[],[]]);
