const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='sudoku'></div></body></html>";
let dom2 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='sudoku'><div/></div></body></html>";
let dom3 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='sudoku'><div class='sudokuSquare'><span/><span/></div></div></body></html>";

function runTest(fun, dom, args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

function newGame() {
    var obj = document.getElementById('sudoku');
    var subObjects = obj.getElementsByTagName('DIV');
    console.log("1");
    for (var no = 0; no < subObjects.length; no++) {
	console.log("2");
        if (subObjects[no].className == 'sudokuSquare') {
	    console.log("3");
	    subObjects[no].style.backgroundColor = '';
            var spans = subObjects[no].getElementsByTagName('SPAN');
            spans[0].style.display = 'none';
            spans[1].innerHTML = '';
        }
	console.log("4");
    }
    console.log("5");

}

// all branches: (1,2), (1,5), (2,3), (2, 4)

// cover branch (1,5); path = [1,5]
runTest(newGame, dom1);

// cover branch (1,2), (2,4); path = [1,2,4,5]
runTest(newGame, dom2);

// cover branch (1,2), (2,3); path = [1,2,3,4,5]
runTest(newGame, dom3);
