function safeAdd(frameid) {
    console.log(this)
    var iframe = document.createElement("iframe");
    var anchor = document.getElementById("node");
    var frame = document.getElementById(frameid);
    iframe.setAttribute("id", frameid);
    if (frame) { 
        //iframe.removeChild(frame.childNodes[0]);
        frame.parentNode.removeChild(frame); 
    } else {
        anchor.appendChild(iframe)
    }
};
