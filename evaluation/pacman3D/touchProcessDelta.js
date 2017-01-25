function touchProcessDelta(dx, dy, minDelta) {
  var adx=Math.abs(dx);
  var ady=Math.abs(dy);
  if (adx>ady) {
    if (adx>minDelta) movedir=(dx>0)? 1:2;
  }
  else {
    if (ady>minDelta) movedir=(dy>0)? 8:4;
  }
}