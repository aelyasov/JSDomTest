/*t dom : string */
function test(str) {
  var div = document.getElementById("status");
  while (div.childNodes.length > 0) {
    div.removeChild(div.childNodes[0]);
  }
  div.appendChild(document.createTextNode(str));
}