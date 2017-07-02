/*t [int]  */
function test(ccard) {
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