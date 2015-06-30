var fondue = require('fondue'),
    vm = require('vm');

var src = fondue.instrument('function foo(a) { return a * 2 }; foo(4)');
var sandbox = { __tracer: undefined };
var output = vm.runInNewContext(src, sandbox);
// var tracer = sandbox.__tracer; // created by fondue when instrumented code is run
