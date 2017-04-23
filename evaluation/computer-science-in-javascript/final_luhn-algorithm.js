/*t [int]:int:int  */
function test(identifier, alt) {
    var sum = 0,
        i = identifier.length - 1,
        num;
    while (i >= 0) {
        num = parseInt(identifier.charAt(i), 10);
        if (isNaN(num)) {
            return false;
        }
        if (alt) {
            num *= 2;
            if (num > 9) {
                num = (num % 10) + 1;
            }
        }
        alt = !alt;
        sum += num;
        i--;
    }
    return (sum % 10 == 0);
}