const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='LEVEL1'></div><div id='LEVEL2'><div id='SCORE1'></div><div id='SCORE2'></div><div id='SCORE3'></div><div id='SCORE4'><div id='SCORE5'></div></div><div id='LIVES1'></div><div id='LIVES2'></div></body></html>";


function runTest(fun, dom = "", args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

let log = console.log;

function writeText(myLevel,myScore, myLives) {
    //Do Level
    //Int to Str
    var stringText = myLevel + '';
    //Pad string
    var i = 0;
    log(1);
    for (i = 1; i <= 2; i++) {
	log(2);
        if (stringText.length < 2) {
	    log(3);
            stringText = '0' + stringText;
        }
	log(4);
    }
    log(5);
    for (i = 1; i <= 2; i++) {
	log(6);
        document.getElementById('LEVEL' + i).src = "img/numbers" + stringText.charAt(i - 1) + ".png";
    }
    //Do Score
    //Int to Str
    stringText = myScore + '';
    //Pad string
    log(7);
    for (i = 1; i <= 5; i++) {
	log(8);
        if (stringText.length < 5) {
	    log(9);
            stringText = '0' + stringText;
        }
	log(10);
    }
    log(11);
    for (i = 1; i <= 5; i++) {
	log(12);
        document.getElementById('SCORE' + i).src = "img/numbers" + stringText.charAt(i - 1) + ".png";
    }
    //Do Lives
    //Int to Str
    stringText = myLives + '';
    //Pad string
    log(13);
    for (i = 1; i <= 2; i++) {
	log(14);
        if (stringText.length < 2) {
	    log(15);
            stringText = '0' + stringText;
        }
	log(16);
    }
    log(17);
    for (i = 1; i <= 2; i++) {
	log(18);
        document.getElementById('LIVES' + i).src = "img/numbers" + stringText.charAt(i - 1) + ".png";
    }
    log(19);
}

// all branches: (1,2), (2,3), (2,4), (1,5), (5,6), (5,7), (7,8), (8,9), (8,10), (7,11), (11,12), (11,13), (13,14), (14,15), (14,16), (13,17), (17,18), (17,19)

// cover branch (1,2), (2,3), (2,4), (5,6), (7,8), (8,9), (8,10), (11,12), (13,14), (14,15), (17,18); path = [...]
runTest(writeText, dom1, [0,0,0]);

// cover branch (1,2), (2,4) (5,6), (7,8), (8,9), (8,10), (11,12), (13,14), (14,15), (17,18); path = [...]
//runTest(writeText, dom1, [11,11111,11]);
