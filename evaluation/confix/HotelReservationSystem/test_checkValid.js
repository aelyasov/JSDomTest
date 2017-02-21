const jsdom = require("jsdom");

let doc, window, document;

let dom1 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><input id='c1' checked=true><div id='error_cardno'></div></input></body></html>";

let dom2 = "<!DOCTYPE HTML><html><head><title>Test</title></head><body><input id='c1'></input><input id='c2' checked=true><div id='error_cardno'></div></input></body></html>";

function runTest(fun, dom = "", args = []) {
    doc = jsdom.jsdom(dom);
    window = doc.defaultView;
    document = window.document;
    Reflect.apply(fun, this, args);
}

function checkValid(cardNumber) {

    console.log("1");
    if (document.getElementById('c1').checked) {
	console.log("2");
	if (!isValidVISA(cardNumber)) {
	    console.log("3");
            document.getElementById("error_cardno").innerHTML = "Invalid VISA Card No";
            return false;
        }
	console.log("4");
    }
    console.log("5");

    if (document.getElementById('c2').checked) {
	console.log("6");
        if (!isValidMasterCard(cardNumber)) {
	    console.log("7");
            document.getElementById("error_cardno").innerHTML = "Invalid MasterCard No";
            return false;
        }
	console.log("8");
    }
    console.log("9");

    return true;
}

function isValidVISA(cardNumber) {
    // if you want to accept Visa (Debit/Electron/Credit Card) ONLY!
    if (cardNumber.charAt(0) == '4' && (cardNumber.length == 13 || cardNumber.length == 16)) {
        return isValidCard(cardNumber);
    }

    return false;
}

function isValidMasterCard(cardNumber) {
    // if you want to accept Mastercard.
    if (cardNumber.charAt(0) == '5' && (cardNumber.charAt(1) == '1' || cardNumber.charAt(1) == '5') && cardNumber.length == 16) {
        return isValidCard(cardNumber);
    }
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

// all branches: (1,2), (1,5), (2,3), (2,4), (5,9), (5,6), (6,7), (6,8)

// cover branch (1,2), (2,3); path = [1,2,3]
runTest(checkValid, dom1, [""]);

// cover branch (1,5), (5,6), (6,7); path = [1,5,6,7]
runTest(checkValid, dom2, [""]);

