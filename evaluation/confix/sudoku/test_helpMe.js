const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='square_1_1'><span></span><span></span></div></body></html>";

let dom2 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='square_1_1' style='backgroundColor:red'><span></span><span>a</span></div></body></html>";

function runTest(fun, dom = "", args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

let log = console.log;

function helpMe(row, col) {
    var allreadyRevealed = true;
    var counter = 0;
    do {
	log(1);
        var el = document.getElementById('square_' + row + '_' + col);

        var spans = el.getElementsByTagName('SPAN');
	log(2);
        if (spans[1].innerHTML.length == 0) {
	    log(3);
            spans[1].innerHTML = spans[0].innerHTML;
            spans[1].style.color = '#FF0000';
            allreadyRevealed = false;
        }
	log(4);
        if (el.style.backgroundColor) {
	    log(5);
	    allreadyRevealed = true;
	}
	log(6);
        counter++;
    } while (allreadyRevealed && counter < 500);
    log(7);
}

/* all branches: (2,3), (2,4), (4,5), (4,6), (6,7)*/

// cover branch (-1,-2); path = [1,2,3,4,6,7] 
runTest(helpMe, dom1, [1,1]);

// cover branch (-1,-2); path = [1,2,3,4,6,7]
runTest(helpMe, dom2, [1,1]);
