const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='square_0_0'><span></span></div></body></html>";

let dom2 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='square_0_0'><span></span></div></body></html>";


function runTest(fun, dom = "", args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

let log = console.log;

function showColumnsInGroup(level, countSquares, row, col) {
    var object = document.getElementById('sudoku');
    var cellsRevealed = new Array();
    var numberArray = new Array();
    var groupCountArray = new Array();
    var maxInGroup = 5;
    log(1);
    if (level <= 0) {
	log(2);
	level = 1;
    }
    log(3);
    if (level == 1) {
	log(4);
	maxInGroup = 4;
    }
    log(5);
    for (var no = 0; no < countSquares[level]; no++) {
	log(6);
        do {
            var obj = document.getElementById('square_' + row + '_' + col);
            var parentID = obj.parentNode.id;
            var span = obj.getElementsByTagName('SPAN')[0];
	    log(7);
            if (!numberArray[span.innerHTML]) {
		log(8);
		numberArray[span.innerHTML] = 0;
	    }
	    log(9);
            if (!groupCountArray[parentID]) {
		log(10);
		groupCountArray[parentID] = 0;
	    }
	    log(11);
        } while (cellsRevealed[row + '_' + col] || numberArray[span.innerHTML] > (3 + Math.ceil(level / 2)) || groupCountArray[parentID] >= maxInGroup);
        cellsRevealed[row + '_' + col] = true;
	log(12);
        if (!numberArray[span.innerHTML]) {
	    log(13);
	    numberArray[span.innerHTML] = 0;
	}
	log(14);
        numberArray[span.innerHTML]++;
        groupCountArray[parentID]++;
        //showCell(obj);

    }
    log(15);
}


/*
 * Types: 
 * level: int
 * countSquares
 */

function showCell(inputDiv) {
    var span = inputDiv.getElementsByTagName('SPAN')[0];
    span.style.display = '';
    inputDiv.style.backgroundColor = '#DDD';
    span.style.color = '#317082';
    var typingSpan = inputDiv.getElementsByTagName('SPAN')[1];
    typingSpan.style.display = 'none';

}

/* all branches: 
   (1,2) *
   (1,3) * 
   (3,4) *
   (3,5) *
   (5,6) *
   (5,15) *
   (7,8) *
   (7,9) 
   (9,10) *
   (9,11) 
   (12,13) *
   (12,14) 
*/


// cover branch (1,2), (3,4), ; path = [1,2,3,4,5,15] 
runTest(showColumnsInGroup, dom1, [-1,[],0,0]);

// cover branch (1,3), (3,5); path = [1,3,5,15] 
runTest(showColumnsInGroup, dom1, [2,[],0,0]);

// cover branch (1,2), (3,4), (5,6), (7,8), (9,10); path = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15] 
runTest(showColumnsInGroup, dom1, [0,[1,1],0,0]);

// cover branch (1,2), (3,4), (5,6), (7,8), (9,10); path = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15] 
runTest(showColumnsInGroup, dom2, [0,[1,2],0,0]);
