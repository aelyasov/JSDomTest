const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body></body></html>";

let dom2 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div><span>5</span></div></body></html>";

function runTest(fun, dom, args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

function shuffleBoard() {
    var squareObjects = document.getElementsByTagName("DIV");
    console.log("1");
    for (var counter = 0; counter < 30; counter++) {
	console.log("2");
        var number1 = Math.ceil(Math.random() * 9);
        var number2 = Math.ceil(Math.random() * 9);
	console.log("3");
        while (number2 == number1) {
	    console.log("4");
            number2 = Math.ceil(Math.random() * 9);
        }
	console.log("5");

        var tmpObjects1 = new Array();
        var tmpObjects2 = new Array();

	console.log("6");
        for (var no = 0; no < squareObjects.length; no++) {
	    console.log("7");
            var txtObj = squareObjects[no].getElementsByTagName('SPAN')[0];
            if (txtObj.innerHTML == number1) {
		console.log("8");
		tmpObjects1.push(txtObj);
	    }
	    console.log("9");
            if (txtObj.innerHTML == number2) {
		console.log("10");
		tmpObjects2.push(txtObj);
	    }
	    console.log("11");
        }
	console.log("12");

        for (var n1 = 0; no < tmpObjects1.length; no++) {
	    console.log("13");
            tmpObjects1[n1].innerHTML = number2;
            tmpObjects2[n1].innerHTML = number1;
        }
	console.log("14");
    }
    console.log("15");
}

// all branches: (1,8), (1,2), (2,3), (2,7), (4,5), (4,6), (8,9), (8,10)

// cover branch ; path = [1,(2,3,5,6,12,14)^30,15]
runTest(shuffleBoard, dom1);

runTest(shuffleBoard, dom2);
