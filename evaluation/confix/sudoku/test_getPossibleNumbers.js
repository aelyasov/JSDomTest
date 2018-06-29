const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='test_1_1'><span></span><span></span></div></body></html>";

let dom2 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='test_1_1'><span>a</span><span>b</span></div><div id='square_1_0'><span></span><span></span></div><div id='square_0_1'><span></span><span></span></div><div id='square_1_1'><span></span><span></span></div></body></html>";

let dom3 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><h1 id='test_1_1'><span>a</span><span>b</span></h1><span id='square_1_0'><span style='display:none'>1</span><span>2</span></span><span id='square_0_1'><span></span><span></span></span><span id='square_1_1'><span style='display:none'>1</span><span>2</span></span></body></html>";

let dom4 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='test_1_1'><span>a</span><span>b</span></div><div id='square_1_0'><span>0</span><span>2</span></div></body></html>";


function runTest(fun, dom = "", args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

let log = console.log;

function getPossibleNumbers() {

    var inputObj = document.getElementById("test_1_1");

    var noArray = new Array();
    var countNumbers = 0;
    var spans = inputObj.getElementsByTagName('SPAN');
    log(-1);
    if (spans[0].innerHTML == spans[1].innerHTML) {
	log(-2);
	return 0;
    }

    var parentDiv = inputObj.parentNode;
    var subDivs = parentDiv.getElementsByTagName('DIV');
    log(1);
    for (var no = 0; no < subDivs.length; no++) {
	log(2);
        if (subDivs[no] != inputObj) {
	    log(3);
            var spans = subDivs[no].getElementsByTagName('SPAN');
            if (spans[0].innerHTML == spans[1].innerHTML || subDivs[no].style.backgroundColor.length > 1) {
		log(4);
                if (!noArray[spans[0].innerHTML]) {
		    log(5);
                    noArray[spans[0].innerHTML] = true;
                    countNumbers++;
                }
		log(6);
            }
	    log(7);
        }
	log(8);
    }
    log(9);

    var numbers = inputObj.id.split('_');
    var row = numbers[1];
    var col = numbers[2];

    log(10);
    for (var no = 0; no < 2; no++) {
	
        var obj = document.getElementById('square_' + row + '_' + no);
	log(11);
        if (obj != inputObj) {
	    log(12);
            var spans = obj.getElementsByTagName('SPAN');
            if (spans[0].innerHTML == spans[1].innerHTML || !spans[0].style.display) {
		log(13);
		log(spans[0].innerHTML);
		log(noArray);
		if (!noArray[spans[0].innerHTML]) {
		    log(14);
                    noArray[spans[0].innerHTML] = true;
                    countNumbers++;
                }
		log(15);
            }
	    log(16);
        }
	log(17);

        var obj = document.getElementById('square_' + no + '_' + col);
	log(18);
        if (obj != inputObj) {
	    log(19);
            var spans = obj.getElementsByTagName('SPAN');
	    log(20);
            if (spans[0].innerHTML == spans[1].innerHTML || !spans[0].style.display) {
		log(21);
                if (!noArray[spans[0].innerHTML]) {
		    log(22);
                    noArray[spans[0].innerHTML] = true;
                    countNumbers++;
                }
		log(23);
            }
	    log(24);
        }
	log(25);
    }
    log(26);
    return countNumbers;
}

/* all branches: 
   (-1,-2) * 
   (-1,1) *
   (1,2) *
   (2,3) *
   (3,4) *
   (4,5) *
   (4,6) *
   (3,7)
   (2,8) *
   (1,9) *
   (10,11) *
   (11,12) *
   (12,13) *
   (13,14) *
   (13,15) 
   (12,16) *
   (11,17)
   (18,19) * 
   (20,21) 
   (21,22) 
   (21,23) *
   (20,24) *
   (18,25)
   (10,26)
*/

// cover branch (-1,-2); path = [-1,-2]
// runTest(getPossibleNumbers, dom1, []);

// cover branch (-1,1), (1,2), (2,8), (2,3), (3,4), (4,5), (4,6), (10,11), (11,12), (12,13), (18,19), (21, 23); path = [1,2,8,2,3,4,5,6,7,8,2,3,4,6,7,8,2,3,4,6,7,8,9,10,11,12,13,15,16,17,18,19,20,21,23,24,25,11,12,13,15,16,17,18,19,20,21,23,24,25,26]
// runTest(getPossibleNumbers, dom2, []);

// cover branch (-1,1), (1,9), (10,11), (11,12), (12,16), (18,19), (20,24); path = [-1,1,9,10,11,12,16,17,18,19,20,21,22,23,24,25,11,12,16,17,18,19,20,24,25,26]
// runTest(getPossibleNumbers, dom3, []);


// cover branch (-1,1), (1,9), (10,11), (11,12), (12,16), (18,19), (20,24); path = [-1,1,9,10,11,12,16,17,18,19,20,21,22,23,24,25,11,12,16,17,18,19,20,24,25,26]
runTest(getPossibleNumbers, dom4, []);




