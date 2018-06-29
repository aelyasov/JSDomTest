/*t dom : int : int : int */
function test(a, b, c) {
  var type, t;
  if (a > b) {
    t = a;
    a = b;
    b = t;
  }
  if (a > c) {
    t = a;
    a = c;
    c = t;
  }
  if (b > c) {
    t = b;
    b = c;
    c = t;
  }
  if (a + b <= c) {
    type = "NOT_A_TRIANGLE";
  } else {
    type = "SCALENE";
    if (a == b && b == c) {
      type = "EQUILATERAL";
    }
    else {
      if (a == b || b == c) {
        type = "ISOSCELES";
      }
    }
  }
  return type;
}