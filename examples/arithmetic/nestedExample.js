/*t dom : int : int : int */
function test(a, b, c) {
  // console.log("a = ", a, typeof(a));
  // console.log("b = ", b, typeof(b));
  // console.log("c = ", c, typeof(c));
  if (a == b) {
    if (b == c) {
      if (c < 0) {
        return 0;
      }
    }
  }
}