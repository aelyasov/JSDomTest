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


function isValidIdentifier(identifier, alt) {

    var sum = 0,
        i = identifier.length - 1,
        num;
    log(1);
    while (i >= 0) {
	log(2);
        //get the next digit
        num = parseInt(identifier.charAt(i), 10);

        //if it's not a valid number, abort
        if (isNaN(num)) {
	    log(3);
            return false;
        }
	log(4);
        //if it's an alternate number...
        if (alt) {
	    log(5);
            num *= 2;
            if (num > 9) {
		log(6);
                num = (num % 10) + 1;
            }
	    log(7);
        }
	log(8);

        //flip the alternate bit
        alt = !alt;

        //add to the rest of the sum
        sum += num;

        //go to next digit
        i--;
    }
    log(9);

    //determine if it's valid
    return (sum % 10 == 0);
}

/* branhes: (1,2), (2,3), (2,4), (4,5), (4,8), (5,6), (5,7) */

// cover branch (1,2), (2,3); path = [1,2,3]
runTest(isValidIdentifier, dom1, ["abc", false]);

// cover branch (1,2), (2,4), (4,8); path = [1,2,4,8,9]
runTest(isValidIdentifier, dom1, ["1", false]);

// cover branch (1,2), (2,4), (4,5), (5,6); path = [1,2,4,5,6,7,8,9]
runTest(isValidIdentifier, dom1, ["5", true]);

// cover branch (1,2), (2,4), (4,5), (5,7); path = [1,2,4,5,7,8,9]
runTest(isValidIdentifier, dom1, ["1", true]);
