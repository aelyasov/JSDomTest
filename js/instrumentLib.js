var _K_ = 1,
    _branchDistance_ = [],
    _trace_ = [1];

function abs(x, y) {
    var typeX = typeof(x);
    var typeY = typeof(y);
    if (typeX == typeY) {
	switch (typeX) {
	    case "number" : return Math.abs(x-y);
	    case "string" : return Math.abs(x.length-y.length); // replace to hamming distance function
	    case "boolean": return Math.abs(x-y);
	    case "object" : return Number(x == y);
	    default: return 0;
	}
    } else {
	return 1;
    }
}


function absZero(x) {
    var typeX = typeof(x);
    switch (typeX) {
	default: return _K_;
    }
}


function absNegZero(x) {
    var typeX = typeof(x);
    switch (typeX) {
    case "number" : return Math.abs(x);
    case "string" : return Math.abs(x.length);
    default: return _K_;
    }
}


module.exports._K_ = _K_;
module.exports._branchDistance_ = _branchDistance_;
module.exports._trace_ = _trace_;
module.exports.abs = abs;
module.exports.absZero = absZero;
module.exports.absNegZero = absNegZero;


