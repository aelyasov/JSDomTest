/*t dom:string */
function test(cardNumber) {
    if (document.getElementById('c1').checked) {
	if (!isValidVISA(cardNumber)) {
            document.getElementById("error_cardno").innerHTML = "Invalid VISA Card No";
            return false;
        }
    }

    if (document.getElementById('c2').checked) {
        if (!isValidMasterCard(cardNumber)) {
            document.getElementById("error_cardno").innerHTML = "Invalid MasterCard No";
            return false;
        }
    }

    return true;
}

function isValidVISA(cardNumber) {
    if (cardNumber[0] == 4 && (cardNumber.length == 13 || cardNumber.length == 16)) {
        return isValidCard(cardNumber);
    }

    return false;
}

function isValidMasterCard(cardNumber) {
    if (cardNumber[0] == 5 && (cardNumber[1] == 1 || cardNumber[1] == 5) && cardNumber.length == 16) {
        return isValidCard(cardNumber);
    }
    return false;
}

function isValidCard(ccard) {
    var sum = 0;
    var i;

    if (ccard.length < 11) {
        return false;
    }

    for (i = 0; i < ccard.length; i = i + 2) {
        ccard[i] = ccard[i] * 2;
        if (ccard[i] > 9) {
            ccard[i] = ccard[i] - 9;
        }
    }
    for (i = 0; i < ccard.length; i++) {
        sum = sum + ccard[i];
    }
    return ((sum % 10) == 0);
}
