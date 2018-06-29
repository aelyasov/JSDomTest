/*t float */
function test(n) {
    var g = 4.7421875;
    var t, x;
    if (isInteger(n)) {
        if (n <= 0) {
            return isFinite(n) ? Infinity : NaN;
        }

        if (n > 171) {
            return Infinity; 
        }

        var value = n - 2;
        var res = n - 1;
        while (value > 1) {
            res *= value;
            value--;
        }

        if (res == 0) {
            res = 1; 
        }

        return res;
    }

    if (n < 0.5) {
        return Math.PI / (Math.sin(Math.PI * n)); 
    }

    if (n >= 171.35) {
        return Infinity; 
    }

    if (n > 85.0) {
        var twoN = n * n;
        var threeN = twoN * n;
        var fourN = threeN * n;
        var fiveN = fourN * n;
        return Math.sqrt(2 * Math.PI / n) * Math.pow((n / Math.E), n) *
            (1 + 1 / (12 * n) + 1 / (288 * twoN) - 139 / (51840 * threeN) -
                571 / (2488320 * fourN) + 163879 / (209018880 * fiveN) +
                5246819 / (75246796800 * fiveN * n));
    }

    --n;
    x = p[0];
    for (var i = 1; i < p.length; ++i) {
        x += p[i] / (n + i);
    }

    t = n + g + 0.5;
    return Math.sqrt(2 * Math.PI) * Math.pow(t, n + 0.5) * Math.exp(-t) * x;
}

var p = [
    0.99999999999999709182,
    57.156235665862923517
];


function isInteger(value) {
    return isFinite(value) ?
        (value == Math.round(value)) :
        false;
};