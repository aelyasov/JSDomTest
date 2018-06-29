const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><div id='inputDiv'><span/><span/></div></body></html>";


function runTest(fun, dom = "", args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

let log = console.log;

function showCell() {
    log(1);
    var inputDiv = document.getElementById("inputDiv");
    var span = inputDiv.getElementsByTagName('SPAN')[0];
    span.style.display = '';
    inputDiv.style.backgroundColor = '#DDD';
    span.style.color = '#317082';
    var typingSpan = inputDiv.getElementsByTagName('SPAN')[1];
    typingSpan.style.display = 'none';
    log(2);
}



// cover branch (1,2); path = [1,2]
runTest(showCell, dom1, []);
