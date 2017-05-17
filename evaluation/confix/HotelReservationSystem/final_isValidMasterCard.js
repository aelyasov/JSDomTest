/*t string */
function test(cardNumber) {
    if (cardNumber.charAt(0) == '5' && (cardNumber.charAt(1) == '1' || cardNumber.charAt(1) == '5') && cardNumber.length == 16) {
        return isValidCard(cardNumber);
    }
    return false;
}

function isValidCard(cardNumber) {
    var ccard = new Array(cardNumber.length);
    var i = 0;
    var sum = 0;

    if (cardNumber.length < 11) {
        return false;
    }

    for (i = 0; i < cardNumber.length; i++) {
        ccard[i] = parseInt(cardNumber.charAt(i));
    }

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