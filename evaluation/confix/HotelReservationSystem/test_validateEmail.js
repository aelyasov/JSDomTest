const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><form id='frm'><input id='email' /><div id='error_email'><div></form></body></html>";

let dom2 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><form id='frm'><input id='email' value='test@test.com'></input><div id='error_email'><div></form></body></html>";

function runTest(fun, dom = "", args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

function validateEmail() {
    var x = document.forms["frm"].elements["email"].value;
    var atpos = x.indexOf("@");
    var dotpos = x.lastIndexOf(".");
    console.log(1);
    if (atpos < 1 || dotpos < atpos + 2 || dotpos + 2 >= x.length) {
	console.log(2);
        document.getElementById("error_email").innerHTML = "* Invalid email id";
        return false;
    }
    console.log(3);
    return true;
}


// all branches: (1,2), (1,3)

// cover branch (1,2); path = [1,2]
runTest(validateEmail, dom1);

// cover branch (1,3); path = [1,3]
runTest(validateEmail, dom2);
