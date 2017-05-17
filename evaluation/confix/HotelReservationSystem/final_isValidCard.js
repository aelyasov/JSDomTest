/*t string  */
function test(cardNumber) {
    var ccard = new Array(cardNumber.length);
    var sum = 0;

    if (cardNumber.length < 11) {
        return false;
    }
    for (var i = 0; i < cardNumber.length; i++) {
        ccard[i] = parseInt(cardNumber.charAt(i));
    }
    for (var i = 0; i < cardNumber.length; i = i + 2) {
        ccard[i] = ccard[i] * 2;
        if (ccard[i] > 9) {
            ccard[i] = ccard[i] - 9;
        }
    }
    for (var i = 0; i < cardNumber.length; i++) {
        sum = sum + ccard[i];
    }
    return ((sum % 10) == 0);
}