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

// (int, [int], int, [int], [int], [int], int, int, [int], [int], int, int, int)
function fireMeteor(maxMeteors, meteorStatus, isPaused, meteorX, meteorY, meteorIteration, myWidth, angleRad, meteorVelocityX, meteorVelocityY, meteorVelocity, myLevel, meteorPeriod) {
    var i = 0;
    var launchMe = 0;
    log(1);
    for (i = 1; i <= maxMeteors; i++) {
	log(2);
        //try and find a launch slot
        if (meteorStatus[i] != 1) {
	    log(3);
            launchMe = i;
        }
	log(4);
    }
    log(5);
    if (launchMe > 0 && isPaused == 0) {
	log(6);
	meteorStatus[launchMe] = 1;
        meteorX[launchMe] = (Math.random() * (myWidth + 300)) + 100;
        meteorY[launchMe] = -20;
        meteorIteration[launchMe] = 1;
        angleRad = (135 + (Math.random() * 90)) * (Math.PI / 180);
        meteorVelocityX[launchMe] = (meteorVelocity + myLevel) * Math.sin(angleRad);
        meteorVelocityY[launchMe] = (meteorVelocity + myLevel) * Math.cos(angleRad);
    }
    log(7);
    window.setTimeout("fireMeteor();", meteorPeriod);
}

// all branches: (1,2), (1,5), (2,3), (2,4), (5,6), (5,7)

// cover branch (1,2), (2,3), (5,7); path = [1,2,3,4,5,7]
runTest(fireMeteor, dom1, [1,[],1,[],[],[],1,1,[],[],1,1,1]);

// cover branch (1,5), (5,7); path = [1,5,7]
runTest(fireMeteor, dom1, [0,[],1,[],[],[],1,1,[],[],1,1,1]);

// cover branch (1,2), (2,4), (5,7); path = [1,2,4,5,7]
runTest(fireMeteor, dom1, [1,[1,1],1,[],[],[],1,1,[],[],1,1,1]);

// cover branch (1,2), (2,4), (5,6); path = [1,2,4,5,6,7]
runTest(fireMeteor, dom1, [1,[],0,[],[],[],1,1,[],[],1,1,1]);
