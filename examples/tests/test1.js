/*t dom : string : dom  */
var instrument = require("./instrumentLib.js"); 

function test(frameid, window, document) {
    var anchor = document.getElementById("node1").getElementById("node1");
    if (true) {
	var anchor = document.getElementById("node3");
    } else {
	var anchor = document.getElementById("node4");
    }
}
