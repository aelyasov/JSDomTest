/*t [int] */
function test(cardNumber) {
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
