/*t dom : string */
function test(log) {
  var tr = document.getElementById("myTable").insertRow(1);
  var td1 = tr.insertCell(0);
  var td2 = tr.insertCell(1);
  var d = new Date();
  td1.appendChild(document.createTextNode(d));
  td2.appendChild(document.createTextNode(log))
}