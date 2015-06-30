function sortrows(table, n, comparator) {
  var tbody = table.tBodies[0];
  var rows = tbody.getElementsByTagName("tr");
  rows = Array.prototype.slice.call(rows,0);
  rows.sort(function(row1,row2) {
    var cell1 = row1.getElementsByTagName("td")[n];
    var cell2 = row2.getElementsByTagName("td")[n];
    var val1 = cell1.textContent || cell1.innerText;
    var val2 = cell2.textContent || cell2.innerText;
    if (comparator) return comparator(val1, val2);

    if (val1 < val2) return -1;
    else if (val1 > val2) return 1;
    else return 0;
  });

  for(var i = 0; i < rows.length; i++) tbody.appendChild(rows[i]);
}
