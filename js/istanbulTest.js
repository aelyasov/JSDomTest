var istanbul = require('istanbul'),
    vm = require('vm');

var instrumenter = new istanbul.Instrumenter();
var changed = instrumenter.instrumentSync('function double (x){var y = x;while (y < 2*x) {y++;}return y;}double(5);');
console.log(changed);

var configObj = require('istanbul').config.loadObject();

console.log(configObj.reporting.config);


var result = eval(changed);
console.log(result);
console.log(eval(instrumenter.currentState.trackerVar));

