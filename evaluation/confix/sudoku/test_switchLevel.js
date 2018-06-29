const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body></body></html>";
let dom2 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><a/></body></html>";

function switchLevel(gameFinished) {
    var linkObj = document.getElementsByTagName("BODY")[0];
    var confirmSwitch = gameFinished;
    console.log("1");
    if (confirmSwitch) {
	console.log("2");
        var parentObj = linkObj.parentNode.parentNode;
        var links = parentObj.getElementsByTagName('A');
	console.log("3");
        for (var no = 0; no < links.length; no++) {
	    console.log("4");
            links[no].style.fontWeight = 'normal';
        }
	console.log("5");
        linkObj.style.fontWeight = 'bold';
    }
    console.log("6");
}

function runTest(fun, dom, args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

// all branches: (1,6), (1,2), (3,4), (3,5)

// cover branch (1,2), (3,5); path = [1,2,3,5,6]
runTest(switchLevel, dom1, [true]);

// cover branch (1,2), (3,4); path = [1,2,3,4,5,6]
runTest(switchLevel, dom2, [true]);

// cover branch (1,6); path = [1,6]
runTest(switchLevel, dom2, [false]);
