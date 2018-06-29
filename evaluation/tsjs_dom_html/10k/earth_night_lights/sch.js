function sch(o, v) {
  var h = o.length,l = -1,m;
  while (h - l > 1)
    if (o[m = h + l >> 1] < v)l = m; else h = m;
  return h;
}

/* Types:
 * o: [int]
 * v: int
 */