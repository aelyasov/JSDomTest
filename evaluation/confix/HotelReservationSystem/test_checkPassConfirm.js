const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='pass'></div><div id='c_pass'></div></body></html>";

let dom2 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><input id='pass'></input><input id='c_pass' value=1></input><div id='error_Password'></div></body></html>";

function runTest(fun, dom, args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

function checkPassConfirm() {
    var pass = document.getElementById("pass").value;
    var c_pass = document.getElementById("c_pass").value;
    console.log("1", pass);
    if (pass != c_pass) {
        console.log("2");
        document.getElementById("pass").value = "";
        document.getElementById("c_pass").value = "";
        document.getElementById("error_Password").innerHTML = "Passwords do not match";
        return false;
    }
    console.log("3");
    return true;
}


// all branches: (1,2), (1,3) 

// cover branch (1,3); path = [1,3]
runTest(checkPassConfirm, dom1);

// cover branch (1,2); path = [1,2]
runTest(checkPassConfirm, dom2);
