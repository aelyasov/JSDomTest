const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "";

function runTest(fun, dom = "", args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

let log = console.log;

function gamma(n) {
    var g = 4.7421875;
    var t, x;
    log(1);
    if (isInteger(n)) {
	log(2);
        if (n <= 0) {
	    log(3);
            return isFinite(n) ? Infinity : NaN;
        }
	log(4);

        if (n > 171) {
	    log(5);
            return Infinity; // Will overflow
        }

	log(6);
        var value = n - 2;
        var res = n - 1;
        while (value > 1) {
	    log(7);
            res *= value;
            value--;
        }
	log(8);

        if (res == 0) {
	    log(9);
            res = 1; // 0! is per definition 1
        }
	log(10);

        return res;
    }

    log(11);
    if (n < 0.5) {
	log(12);
        return Math.PI / (Math.sin(Math.PI * n)); // FIX: removed recursion
    }
    log(13);

    if (n >= 171.35) {
	log(14);
        return Infinity; // will overflow
    }
    log(15);

    if (n > 85.0) { // Extended Stirling Approx
	log(16);
        var twoN = n * n;
        var threeN = twoN * n;
        var fourN = threeN * n;
        var fiveN = fourN * n;
        return Math.sqrt(2 * Math.PI / n) * Math.pow((n / Math.E), n) *
            (1 + 1 / (12 * n) + 1 / (288 * twoN) - 139 / (51840 * threeN) -
                571 / (2488320 * fourN) + 163879 / (209018880 * fiveN) +
                5246819 / (75246796800 * fiveN * n));
    }
    log(17);

    --n;
    x = p[0];
    for (var i = 1; i < p.length; ++i) {
	log(18);
        x += p[i] / (n + i);
    }
    log(19);

    t = n + g + 0.5;
    return Math.sqrt(2 * Math.PI) * Math.pow(t, n + 0.5) * Math.exp(-t) * x;
}


// FIXED: removed some elements
var p = [
    0.99999999999999709182,
    57.156235665862923517
];


function isInteger(value) {
    return isFinite(value) ?
        (value == Math.round(value)) :
        false;
    // Note: we use ==, not ===, as we can have Booleans as well
};

/** branhes:  
    (1,2) *
    (2,3) *
    (2,4) *
    (4,5) *
    (4,6) *
    (6,7) *
    (6,8) *
    (8,9) *
    (8,10) * 
    (1,11) * 
    (11,12) *
    (11,13) *
    (13,14) *
    (13,15) *
    (15,16) *
    (15,17) *
    (17,18) *
*/

// cover branch (1,2), (2,3); path = [1,2,3]
runTest(gamma, dom1, [0]);

// cover branch (1,2), (2,4), (4,6), (6,8), (8,9); path = [1,2,4,6,8,9,10]
runTest(gamma, dom1, [1]);

// cover branch (1,2), (2,4), (4,5); path = [1,2,4,5]
runTest(gamma, dom1, [172]);

// cover branch (1,2), (2,4), (6,7), (8,10); path = [1,2,4,6,7,8,10]
runTest(gamma, dom1, [4]);

// cover branch (1,11), (11,12); path = [1,11,12]
runTest(gamma, dom1, [0.4]);

// cover branch (1,11), (11,13), (13,14); path = [1,11,13,14]
runTest(gamma, dom1, [172.5]);

// cover branch (1,11), (11,13), (13,15), (15,17) (17,18); path = [1,11,13,15,17,18,19]
runTest(gamma, dom1, [1.5]);

// cover branch (1,11), (11,13), (13,15), (15,16); path = [1,11,13,15,16]
runTest(gamma, dom1, [87.5]);


