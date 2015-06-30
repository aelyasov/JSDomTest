function embolden(n) {
  if (typeof n == "string") n = document.getElementById(n);
  var parent = n.parentNode;
  var b = document.createElement("b");
  parent.replaceChild(b, n);
  b.appendChild(n);
}

