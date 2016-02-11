/*t dom */
function test() {
  var div_root   = document.getElementsByTagName("div")[0];
  var div_node1  = div_root.getElementsByTagName("div")[0];
  var div_node2  = div_root.getElementsByTagName("div")[3];
  var div_node11 = div_node1.getElementsByTagName("div")[0];
  var div_node12 = div_node1.getElementsByTagName("div")[1];
  var div_node21 = div_node2.getElementsByTagName("div")[0];
  var div_node22 = div_node2.getElementsByTagName("div")[1];
  var text1 = div_node11.innerText;
  var text2 = div_node12.innerText;
  var text3 = div_node21.innerText;
  var text4 = div_node22.innerText;
  return (text1 + text2 + text3 + text4);
}