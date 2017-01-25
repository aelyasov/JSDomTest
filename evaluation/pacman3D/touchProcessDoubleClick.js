function touchProcessDoubleClick(dx, dy, maxDelta, maxTicks) {
  if (Math.abs(dx)<=maxDelta && Math.abs(dy)<=maxDelta) {
    var now=new Date();
    t=now.getTime();
    if (t-touchTime<maxTicks) {
      doPause();
      touchTime=0;
    }
    else {
      touchTime=t;
    }
  }
  else {
    touchTime=0;
  }
}