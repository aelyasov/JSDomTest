const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='error_phnum'></div></body></html>";

function runTest(fun, dom = "", args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

function validateNumber(input) {
    var number = /^[0-9]+$/;
    console.log(1);
    if (input.match(number)) {
	console.log(2);
        return true;
    } else {
	console.log(3);
        document.getElementById("error_phnum").innerHTML = "* " + " Enter Numbers Only";
        return false;
    }
    console.log(4);
}


// all branches: (1,2), (1,3)

// cover branch (1,3); path = [1,3]
runTest(validateNumber, dom1, [""]);

// cover branch (1,2); path = [1,2]
runTest(validateNumber, dom1, ["123"]);
