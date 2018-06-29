const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='sudoku'></div></body></html>";

let dom2 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='sudoku'><div></div></div></body></html>";

let dom3 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='sudoku'><div class='sudokuSquare'><span></span><span></span></div></div></body></html>";

let dom4 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='sudoku'><div class='sudokuSquare'><span>abc</span><span></span></div></div></body></html>";

function isGameFinished() {
    var obj = document.getElementById('sudoku');
    var subDivs = obj.getElementsByTagName('DIV');
    var allOk = true;
    console.log("1");
    for (var no = 0; no < subDivs.length; no++) {
        console.log("2");
        if (subDivs[no].className.indexOf('sudokuSquare') >= 0 && !subDivs[no].style.backgroundColor) {
            console.log("3");
            var spans = subDivs[no].getElementsByTagName('SPAN');
	    console.log("4");
            if (spans[0].innerHTML != spans[1].innerHTML) {
                console.log("5");
                allOk = false;
                break;
            }
            console.log("6");
        }
        console.log("7");
    }
    console.log("8");
    if (allOk) {
	console.log("9");
        console.log('Congratulations! You did it');
    }
    console.log("10");
}

function runTest(fun, dom, args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

// all branches: *(1,8), *(1,2), *(2,3), *(2,7), *(4,5), *(4,6), *(8,9), (8,10)

// cover branch *(1,8), *(8,9); path = [1,8,9,10]
runTest(isGameFinished, dom1);

// cover branch *(1,2), (2,7), (8,9); path = [1,2,7,8,9,10]
runTest(isGameFinished, dom2);

// cover branch *(2,3), *(4,6), (8,9); path = [1,2,3,4,6,7,8,9,10]
runTest(isGameFinished, dom3);

// cover branch (1,2), (2,3), *(4,5), *(8, 10); path = [1,2,3,4,5,8,10]
runTest(isGameFinished, dom4);
