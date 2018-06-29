const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='BRICK0'></div></body></html>";

let dom2 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='BRICK11'></div></body></html>";

let dom3 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='BRICK21'></div></body></html>";

let dom4 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='BRICK31'></div></body></html>";

let dom5= "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='BRICK41'></div></body></html>";

let dom6= "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='BRICK1'></div></body></html>";

let dom7= "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='BRICK51'></div></body></html>";


function runTest(fun, dom = "", args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

let log = console.log;

function initBricks(i, myCenterH, brickV, brickH, brickStat, theLevelArt, levelData) {
    var brickRow = 0;
    var brickCol = 0;
    var thisBrick = '';
    var brickLeft = myCenterH - 420;
    log(1);
    if (i <= 10) {
	log(2);
        brickRow = 70;
        brickCol = brickLeft + ((i - 1) * 85);
    } else if (i > 10 && i <= 20) {
	log(3);
        brickRow = 125;
        brickCol = brickLeft + ((i - 11) * 85);
    } else if (i > 20 && i <= 30) {
	log(4);
        brickRow = 180;
        brickCol = brickLeft + ((i - 21) * 85);
    } else if (i > 30 && i <= 40) {
	log(6);
        brickRow = 235;
        brickCol = brickLeft + ((i - 31) * 85);
    } else {
	log(7);
        brickRow = 290;
        brickCol = brickLeft + ((i - 41) * 85);
    }
    log(8);
    thisBrick = levelData[theLevelArt].charAt(i - 1);
    if (thisBrick == 'x') {
	log(9);
        brickH[i] = brickCol;
        brickV[i] = brickRow;
        brickStat[i] = 1;
        document.getElementById('BRICK' + i).src = "img/brick.png";
        document.getElementById('BRICK' + i).style.left = brickH[i];
        document.getElementById('BRICK' + i).style.top = brickV[i];
    } else if (thisBrick == 'O') {
	log(10);
        brickH[i] = brickCol;
        brickV[i] = brickRow;
        brickStat[i] = 2;
        document.gиetElementById('BRICK' + i).src = "img/solid.png";
        document.getElementById('BRICK' + i).style.left = brickH[i];
        document.getElementById('BRICK' + i).style.top = brickV[i];
    } else if (thisBrick == 'Z') {
	log(11);
        brickH[i] = brickCol;
        brickV[i] = brickRow;
        brickStat[i] = 6;
        document.getElementById('BRICK' + i).src = "img/death.png";
        document.getElementById('BRICK' + i).style.left = brickH[i];
        document.getElementById('BRICK' + i).style.top = brickV[i];
    } else if (thisBrick == 'o') {
	log(12);
        brickH[i] = brickCol;
        brickV[i] = brickRow;
        brickStat[i] = 3;
        document.getElementById('BRICK' + i).src = "img/weakbrick1.png";
        document.getElementById('BRICK' + i).style.left = brickH[i];
        document.getElementById('BRICK' + i).style.top = brickV[i];
    } else {
	log(13);
        brickStat[i] = 0;
        document.getElementById('BRICK' + i).style.left = -200;
        document.getElementById('BRICK' + i).style.top = -100;
    }
    i++;
    log(14);
    if (i <= 50) {
	log(15);
        window.setTimeout("initBricks(" + i + ");", 20);
    }
    log(16);
}


/*
 * i: int
 * myCenterH: int
 * brickH: [int]
 * brickV: [int]
 * brickStat: [int]
 * levelData: [int]
 * theLevelArt: int
 */


// all branches: (1,2), (1,3), (1,4), (1,6), (1,7), (8,9), (8,10), (8,11), (8,12), (8,13), (14,15)

// cover branch (1,2), (14,15); path = [1,2,8,13,14,15,16]
runTest(initBricks, dom1, [0,[],[],[],[],0,["a"]]);

// cover branch (1,3), (14,15); path = [1,3,8,13,14,15,16]
runTest(initBricks, dom2, [11,[],[],[],[],0,["a"]]);

// cover branch (1,4), (14,15); path = [1,4,8,13,14,15,16]
runTest(initBricks, dom3, [21,[],[],[],[],0,["a"]]);

// cover branch (1,6), (14,15); path = [1,6,8,13,14,15,16]
runTest(initBricks, dom4, [31,[],[],[],[],0,["a"]]);

// cover branch (1,7), (14,15); path = [1,2,8,9,14,15,16]
runTest(initBricks, dom6, [1,[],[],[],[],0,["x"]]);

// cover branch (1,2), (8,10), (14,15); path = [1,2,8,10,14,15,16]
runTest(initBricks, dom6, [1,[],[],[],[],0,["O"]]);

// cover branch (1,2), (8,11), (14,15); path = [1,2,8,9,11,15,16]
runTest(initBricks, dom6, [1,[],[],[],[],0,["Z"]]);

// cover branch (1,2), (8,12), (14,15); path = [1,2,8,9,12,15,16]
runTest(initBricks, dom6, [1,[],[],[],[],0,["o"]]);

// cover branch (1,7), (8,13), (14,16); path = [1,7,8,13,14,16]
runTest(initBricks, dom7, [51,[],[],[],[],0,["o"]]);

