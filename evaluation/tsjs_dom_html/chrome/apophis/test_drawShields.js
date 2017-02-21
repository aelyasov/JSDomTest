const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='SHIELD1'></div><div id='SHIELD2'><div id='SHIELD3'><div id='SHIELD4'><div id='SHIELD5'></div></body></html>";

function runTest(fun, dom = "", args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

let log = console.log;

//Draw shields
function drawShields(shieldStat) {
    var i = 0;
    log(1);
    for (i = 1; i <= 5; i++) {
	log(2);
        if (shieldStat[i] > 0) {
	    log(3);
            document.getElementById('SHIELD' + i).src = "img/shield" + shieldStat[i] + ".png";
        } else {
	    log(4);
            document.getElementById('SHIELD' + i).style.top = -500;
        }
    }
    log(5);
}

// all branches: (1,2), (1,5), (2,3), (2,4)

// cover branch (1,2), (2,4); path = [1,2,4,2,4,2,4,2,4,2,4,5]
runTest(drawShields, dom1, [[]]);


// cover branch (1,2), (2,3), (2,4); path = [1,2,3,2,4,2,4,2,4,2,4,5]
runTest(drawShields, dom1, [[1,1]]);

