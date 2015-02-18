var istanbul = require('istanbul');
var instrumenter = new  istanbul.Instrumenter();
var changed = instrumenter.instrumentSync('function double (x){var y = x;while (y < 2*x) {y++;}return y;}double(5);');
console.log(changed);
