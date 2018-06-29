/*t int */
function _pow(x, y) {

  // Alternatively could define a 'realmode' config option or something, but
  // 'predictable' will work for now
  if (config.predictable && !isInteger(y) && x < 0) {
    // Check to see if y can be represented as a fraction
    try {
      var yFrac = fraction(y);
      var yNum = number(yFrac);
      if(y === yNum || Math.abs((y - yNum) / y) < 1e-14) {
        if(yFrac.d % 2 === 1) {
          return (yFrac.n % 2 === 0 ? 1 : -1) * Math.pow(-x, y);
        }
      }
    }
    catch (ex) {
      // fraction() throws an error if y is Infinity, etc.
    }
    
    // Unable to express y as a fraction, so continue on
  }
  
  if (isInteger(y) || x >= 0 || config.predictable) {
    return Math.pow(x, y);
  }
  else {
    return new type.Complex(x, 0).pow(y, 0);
  }
}