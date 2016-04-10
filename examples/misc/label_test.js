/*t dom */
function test() {
    var y = x;
    while (true) {
      if (x > 0) {
        y = y  + 1;
        x = x - 1;
      } else {
        break;
      }
    }
  return y;
}
