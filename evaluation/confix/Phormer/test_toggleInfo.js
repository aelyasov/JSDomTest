const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='hin'>Show</div><div id='photoBoxes'></div><div id='theImage'></div></body></html>";

let dom2 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='hin'></div><div id='photoBoxes'></div><div id='theImage'></div></body></html>";

let dom3 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='hin'></div><div id='photoBoxes'></div><div id='theImage'></div></body></html>";

function runTest(fun, dom, args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

function toggleInfo(wut) {
    console.log("1");
    if ((!wut) || (wut == '')) {
        console.log("2");
        wut = document.getElementById('hin').innerHTML;
    }
    console.log("3");
    if (wut == 'Show') {
        console.log("4");
        document.getElementById('hin').innerHTML = 'Hide&nbsp;';
        document.getElementById('photoBoxes').style.display = 'block';
        document.getElementById('theImage').style.cssFloat = 'left';
        document.getElementById('theImage').style.styleFloat = 'left';
        document.getElementById('theImage').style.marginRight = '15px';
    } else {
        console.log("5");
        document.getElementById('hin').innerHTML = 'Show';
        document.getElementById('photoBoxes').style.display = 'none';
        document.getElementById('theImage').style.cssFloat = 'none';
        document.getElementById('theImage').style.styleFloat = 'none';
        document.getElementById('theImage').style.marginRight = '55px';
    }
    console.log("6");
}

// all branches: (1,2), (1,3), (3,4), (3,5)

// cover branch (1,2), (3,4); path = [1,2,3,4,6]
runTest(toggleInfo, dom1, ['']);

// cover branch (1,3), (3,4); path = [1,3,4,6]
runTest(toggleInfo, dom2, ['Show']);

// cover branch (1,3), (3,5); path = [1,3,5,6]
runTest(toggleInfo, dom3, ['Test']);
