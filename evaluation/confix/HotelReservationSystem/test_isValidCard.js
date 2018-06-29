const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "";

function runTest(fun, dom = "", args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

function isValidCard(cardNumber) {
    var ccard = new Array(cardNumber.length);
    var i = 0;
    var sum = 0;

    // 6 digit is issuer identifier
    // 1 last digit is check digit
    // most card number > 11 digit
    console.log("1");
    if (cardNumber.length < 11) {
        console.log("2");
        return false;
    }
    console.log("3");
    // Init Array with Credit Card Number
    for (i = 0; i < cardNumber.length; i++) {
        console.log("4");
        ccard[i] = parseInt(cardNumber.charAt(i));
    }
    console.log("5");
    // Run step 1-5 above above
    console.log("6");
    for (i = 0; i < cardNumber.length; i = i + 2) {
        console.log("7");
        ccard[i] = ccard[i] * 2;
        if (ccard[i] > 9) {
            console.log("8");
            ccard[i] = ccard[i] - 9;
        }
        console.log("9");
    }
    console.log("10");
    for (i = 0; i < cardNumber.length; i++) {
        console.log("11");
        sum = sum + ccard[i];
    }
    console.log("12");
    return ((sum % 10) == 0);
}

// all branches: (1,2), (1,3), (3,4), (3,5), (6,7), (6,10), (7,8), (7,9), (10,11), (10,12) 

// cover branch (1,2); path = [1,2]
runTest(isValidCard, dom1, [""]);

// cover branch (1,3), (3,4), (6,7), (7,8), (7,9), (10,11); path = [1,2]
runTest(isValidCard, dom1, ["12345678910"]);
