const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='indicator'></div></body></html>";

let dom2 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='indicator'>60</div></body></html>";

let dom3 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='indicator'>600</div></body></html>";

function runTest(fun, dom, args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

function updateIndic(isAjaxing) {
    var v = document.getElementById('indicator').innerHTML;
    var l = v.length;
    var neck = 2;
    console.log("1");
    if (l > neck) {
        console.log("2");
        v = v.substring(0, l - 3 * 7);
    }
    console.log("3");
    if ((l % 3) == 0) {
        console.log("4");
        document.getElementById('indicator').innerHTML = '&#149;      ' + v;
    } else {
        console.log("5");
        document.getElementById('indicator').innerHTML = '&nbsp; ' + v;
    }
    console.log("6");
    if (isAjaxing) {
        console.log("7");
        //setTimeout("updateIndic();", 500);
    } else {
        console.log("8");
        document.getElementById('indicator').innerHTML = '';
    }
    console.log("9");
}

// all branches: (1,2), (1,3), (3,4), (3, 5), (6,7), (6,8) 

// cover branch (1,3), (3,4), (6,7); path = [1,3,4,6,7,9]
runTest(updateIndic, dom1, [true]);

// cover branch (1,3), (3,4), (6,8); path = [1,3,4,6,8,9]
runTest(updateIndic, dom1, [false]);

// cover branch (1,3), (3,5), (6,7); path = [1,3,5,6,7,9]
runTest(updateIndic, dom2, [true]);

// cover branch (1,2), (3,4), (6,7); path = [1,2,3,4,6,7,9]
runTest(updateIndic, dom3, [true]);
