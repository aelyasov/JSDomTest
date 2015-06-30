
// ---------------------------------------------------------------------------
function insertAt(parent, child, n) {
  if (n < 0 || n > parent.childNodes.length) throw new Error("invalid index");
  else if (n == parent.childNodes.length) parent.appendChild(child);
  else parent.insertBefore(child, parent.childNodes[n]);
}
// ---------------------------------------------------------------------------


function sortrows(table, n, comparator) {
  var tbody = table.tBodies[0];
  var rows = tbody.getElementsByTagName("tr");
  rows = Array.prototype.slice.call(rows,0);
  rows.sort(function(row1,row2) {
    var cell1 = row1.getElementsByTagName("td")[n];
    var cell2 = row2.getElementsByTagName("td")[n];
    var val1  = cell1.textContent || cell1.innerText;
    var val2  = cell2.textContent || cell2.innerText;
    if (comparator) return comparator(val1, val2);

    if (val1 < val2) return -1;
    else if (val1 > val2) return 1;
    else return 0;
  });

  for(var i = 0; i < rows.length; i++) tbody.appendChild(rows[i]);
}
// ---------------------------------------------------------------------------


function embolden(n) {
  if (typeof n == "string") n = document.getElementById(n);
  var parent = n.parentNode;
  var b = document.createElement("b");
  parent.replaceChild(b, n);
  b.appendChild(n);
}
// ---------------------------------------------------------------------------


function reverse(n) {
  var f = document.createDocumentFragment();

  while(n.lastChild) f.appendChild(n.lastChild);
  n.appendChild(f);
}
// ---------------------------------------------------------------------------
