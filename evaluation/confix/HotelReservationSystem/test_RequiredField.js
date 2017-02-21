const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='indicator'></div></body></html>";

let dom2 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='indicator'></div><div id='error_ '></div></body></html>";

function runTest(fun, dom, args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

function RequiredField(felements) {
    var i, field, value;
    console.log("1");
    for (i = 0; i < felements.length; i++) {
        console.log("2");
        field = felements[i];
        value = trim(felements[i]);
        console.log("3");
        if (value == "" || value == null) {
            console.log("4");
            document.getElementById("error_" + field).innerHTML = "* " + field + " Required";
            return false;
        }
        console.log("5");
    }
    console.log("6");
    return true;
}

function trim(s) {
    return s.replace(/^\s*/, "").replace(/\s*$/, "");
}

// all branches: (1,2), (1,6), (3,4), (3, 5) 

// cover branch (1,6); path = [1,6]
runTest(RequiredField, dom1, [[]]);

// cover branch (1,6); path = [1,2,3,5,6]
runTest(RequiredField, dom1, [["a"]]);

// cover branch (1,6); path = [1,2,3,4]
runTest(RequiredField, dom2, [[" "]]);
