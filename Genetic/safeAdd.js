/*t dom : string : dom  */
var instrument = require("./instrumentLib.js"); 

function safeAdd(frameid, window, document) {
    var iframe = document.createElement("iframe");
    var anchor = document.getElementById("node");
    var frame  = document.getElementById(frameid);
    iframe.setAttribute("id", frameid);
    if (frame) { 
        frame.parentNode.removeChild(frame); 
    } else {
	iframe.appendChild(anchor);
    }
}