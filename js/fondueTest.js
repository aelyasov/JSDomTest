var fondue = require('fondue');
var  vm = require('vm');

var src = fondue.instrument('function foo(a) { return a * 2 }; foo(4)');
var sandbox = { __tracer: undefined };
var output = vm.runInNewContext(src, sandbox);
var tracer = sandbox.__tracer; // created by fondue when instrumented code is run

var functions = {};
var nodesHandle = tracer.trackNodes();
tracer.newNodes(nodesHandle).forEach(function (n) {
    if (n.type === 'function') {
        functions[n.name] = n;
    }
});

var fooNode = functions['foo'];
console.log('foo started at', fooNode.start, 'and ended at', fooNode.end);

// call tracer.newNodes() periodically if you expect new code to be required over time
