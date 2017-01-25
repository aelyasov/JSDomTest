/*t int */
function test(x) {
  if (x < 2){
    return false;
  }
  if (x == 2){
    return true;
  }
  if (x % 2 == 0){ 
    return false;
  }
  for (var i = 3; i * i <= x; i += 2){
    if (x % i == 0){
      return false;
    }
  }
  return true;
}