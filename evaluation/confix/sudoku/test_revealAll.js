const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='square_0_0'><span/><span/></div><div id='square_0_1'><span/><span/></div><div id='square_1_0'><span/><span/></div><div id='square_1_1'><span/><span/></div></body></html>";

function runTest(fun, dom, args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

function revealAll() {
    console.log("1");
    for (var row = 0; row < 2; row++) {
	console.log("2");
        for (var col = 0; col < 2; col++) {
	    console.log("3");
            var obj = document.getElementById('square_' + row + '_' + col);
            var spans = obj.getElementsByTagName('SPAN');
            spans[0].style.display = '';
            spans[1].style.display = 'none';
            spans[1].style.color = '#000000';
        }
	console.log("4");
    }
    console.log("5");
}

// all branches: (1,2), (1,5), (2,3), (2, 4)

// cover branch (1,5); path = [1,2,3,3,4,2,3,3,4,5]
runTest(revealAll, dom1);
