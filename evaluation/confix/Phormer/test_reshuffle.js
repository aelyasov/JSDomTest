const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='thumbscount'></div></body></html>";
let dom2 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><input id='thumbscount' value='2'><div id='ThumbInBox0'></div><div id='ThumbInBox1'></div></input></body></html>";

function runTest(fun, dom, args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

function reshuffle() {
    var maxRand = 400 - 75;
    var n = document.getElementById('thumbscount').value;
    console.log("1: ");
    for (var i = 0; i < n; i++) {
	console.log("2");	
        document.getElementById('ThumbInBox' + i).style.top = rand(maxRand) + 'px';
        document.getElementById('ThumbInBox' + i).style.left = rand(maxRand) + 'px';
    }
    console.log("3");
}

function rand(x) {
    return Math.round(Math.random() * x);
}


// all branches: (1,2), (1,3)

// cover branch (1,3); path = [1,3]

runTest(reshuffle, dom1);

// cover branch (1,2); path = [1,2,2,3]
runTest(reshuffle, dom2);
