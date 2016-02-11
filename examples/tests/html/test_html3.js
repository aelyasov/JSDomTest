/*t dom */
function test() {
  var div_root   = document.getElementsByTagName("div")[0];
  var div_node1  = div_root.getElementsByTagName("div")[0];
  var div_node2  = div_root.getElementsByTagName("div")[3];
  var div_node11 = div_node1.getElementsByTagName("div")[0];
  var div_node12 = div_node1.getElementsByTagName("div")[1];
  var div_node21 = div_node2.getElementsByTagName("div")[0];
  var div_node22 = div_node2.getElementsByTagName("div")[1];
  var h1 = div_node11.getElementsByTagName("h1");
  var h2 = div_node12.getElementsByTagName("h2");
  var h3 = div_node21.getElementsByTagName("h3");
  var h4 = div_node22.getElementsByTagName("h4");
  var text1 = h1[0].innerText;
  var text2 = h2[0].innerText;
  var text3 = h3[0].innerText;
  var text4 = h4[0].innerText;
  return (text1 + " " + text2 + " " + text3 + " " + text4);
}