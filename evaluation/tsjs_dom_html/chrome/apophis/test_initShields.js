const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='SHIELD1'/><div id='SHIELD2'/><div id='SHIELD3'/><div id='SHIELD4'/><div id='SHIELD5'/></body></html>";


function runTest(fun, dom = "", args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

let log = console.log;

function initShields(shieldH, myWidth, myHeight) {
    var i = 0;
    log(1);
    for (i = 1; i <= 5; i++) {
	log(2);
        shieldH[i] = (((myWidth - 200) / 5) * (i - 1)) + 150;
        document.getElementById('SHIELD' + i).src = "img/shield1.png";
        document.getElementById('SHIELD' + i).style.top = myHeight - 100;
        document.getElementById('SHIELD' + i).style.left = shieldH[i];
    }
    log(3);
}


// all branches: (1,2)

// cover branch (1,2); path = [1,2,2,2,2,2,5]
runTest(initShields, dom1, [[],0,0]);
