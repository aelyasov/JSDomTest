function test(frameid) {
   var iframe = document.createElement("iframe");
   var anchor = document.getElementById("_" + "node" + "_");
   var frame = document.getElementById(frameid);
   iframe.setAttribute("id",
   frameid);
   if (frame) {
         instrument._trace_.push(6);
         instrument._branchDistance_.push([6
                                          ,instrument.absNegZero(frame)]);
         frame.parentNode.removeChild(frame);
      } else {
         instrument._trace_.push(6);
         instrument._branchDistance_.push([6
                                          ,instrument.absZero(frame)]);
         iframe.appendChild(anchor);
      }
}

