/*t dom */
function test() {
  var div_root   = document.getElementsByTagName("div")[0];
  var div1 = div_root.getElementsByTagName("div")[0];
  var div2 = div_root.getElementsByTagName("div")[2];
  var div3 = div1.getElementsByTagName("div")[0];
  var div4 = div2.getElementsByTagName("div")[0];
  var h11 = div3.getElementsByTagName("h1")[0].innerText;
  var h12 = div4.getElementsByTagName("h1")[0].innerText;
  return (h11 + " " + h12);
}