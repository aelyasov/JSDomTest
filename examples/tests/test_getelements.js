/*t dom  */
function test() {
  var div1 = document.getElementsByTagName("div");
  var div2 = div1[0].getElementsByTagName("div");
  var h1 = div2[0].getElementsByTagName("h1");
  // var div = h3[0].getElementsByTagName("div");
  // return div[0].innerHTML
  return h1[0].innerHTML
}