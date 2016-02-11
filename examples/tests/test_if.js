/*t dom  */
function test() {
  var cells = document.getElementsByTagName("div");
  if (cells[1].length == 0) {
    return document.getElementById("foo");
  } else {
    return document.getElementById("bar");
  }
}
