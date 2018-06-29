const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "";

function runTest(fun, dom = "", args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

function isValidMasterCard(cardNumber) {
    // if you want to accept Mastercard.
    console.log("1");
    if (cardNumber.charAt(0) == '5' && (cardNumber.charAt(1) == '1' || cardNumber.charAt(1) == '5') && cardNumber.length == 16) {
        console.log("2");
        return isValidCard(cardNumber);
    }
    console.log("3");
    return false;
}

function isValidCard(cardNumber) {
    var ccard = new Array(cardNumber.length);
    var i = 0;
    var sum = 0;

    // 6 digit is issuer identifier
    // 1 last digit is check digit
    // most card number > 11 digit
    if (cardNumber.length < 11) {
        return false;
    }
    // Init Array with Credit Card Number
    for (i = 0; i < cardNumber.length; i++) {
        ccard[i] = parseInt(cardNumber.charAt(i));
    }
    // Run step 1-5 above above
    for (i = 0; i < cardNumber.length; i = i + 2) {
        ccard[i] = ccard[i] * 2;
        if (ccard[i] > 9) {
            ccard[i] = ccard[i] - 9;
        }
    }
    for (i = 0; i < cardNumber.length; i++) {
        sum = sum + ccard[i];
    }
    return ((sum % 10) == 0);
}


// all branches: (1,2), (1,3)

// cover branch (1,2); path = [1,3]
runTest(isValidMasterCard, dom1, [""]);

// cover branch (1,3); path = [1,2]
runTest(isValidMasterCard, dom1, ["5100000000000000"]);
