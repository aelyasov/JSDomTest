const winston = require('winston');
const _ = require('underscore');

var _K_ = 1;
var UNKNOWN = 10;
//     _branchDistance_ = [],
//     _trace_ = [1];

function abs(x, y) {
    var typeX = typeof(x);
    var typeY = typeof(y);
    var distance;
    winston.debug("x='" + x + "' is of type " + typeX);
    winston.debug("y='" + y + "' is of type " + typeY);
    if (typeX == typeY) {
        switch (typeX) {
            case "number":
                distance = Math.abs(x - y);
                winston.debug("nummer: " + distance);
                if (Number.isNaN(x) || Number.isNaN(y)) throw new Error("Not a number " + x + y);
                if (!Number.isFinite(x) || !Number.isFinite(y)) throw new Error("Number is not finite " + x + y);
                return distance;
            case "string":
                distance = absString(x, y); //getEditDistance(x, y);
                winston.debug("string: " + distance);
                return distance;
                //case "string" : return naiveHammerDistance(x,y);console.log("str"); break;
            case "boolean":
                distance = Math.abs(x - y);
                winston.debug("boolean: " + distance);
                return distance;
            case "object":
                distance = Number(x == y);
                winston.debug("object: " + distance);
                return distance;
            default:
                winston.debug("unknown type (" + typeX + "): " + UNKNOWN);
                return UNKNOWN;
        }
    } else {
        winston.debug("types don't match");
        return UNKNOWN;
    }
}

function absZero(x) {
    var typeX = typeof(x);
    winston.debug("x=" + x + " is of type " + typeX);
    switch (typeX) {
        case "number":
            winston.debug("nummer: " + _K_);
            return _K_;
        case "string":
            winston.debug("string: " + _K_);
            return _K_;
        case "boolean":
            winston.debug("boolean: " + _K_);
            return _K_;
        case "object":
            winston.debug("object: " + _K_);
            return _K_;
        default:
            winston.debug("default (" + typeX + "): " + UNKNOWN);
            return UNKNOWN;
    }
}


function absNegZero(x) {
    var typeX = typeof(x);
    var distance;
    winston.debug("x=" + x + " is of type " + typeX);
    switch (typeX) {
        case "number":
            distance = Math.abs(x);
            winston.debug("nummer: " + distance);
            return distance;
        case "string":
            distance = absString(x, "");
            winston.debug("string: " + distance);
            return distance;
        case "boolean":
            distance = _K_;
            winston.debug("boolean: " + distance);
            return distance;
        case "object":
            distance = _K_;
            winston.debug("object: " + distance);
            return distance;
        default:
            distance = UNKNOWN;
            winston.debug("default (" + typeX + ") " + distance);
            return distance;
    }
}

function absString(str1, str2) {
    function str2ints(str) {
        var r = [];
        for (var i = 0; i < str.length; i++) {
            r.push(str.charCodeAt(i));
        }
        return r;
    }

    var code = 0;
    var minStrLen = Math.min(str1.length, str2.length);
    var lenDiff = Math.abs(str1.length - str2.length);
    var codes1 = str2ints(str1);
    var codes2 = str2ints(str2);

    for (var i = 0; i < minStrLen; i++) {
        code += Math.abs(codes1[i] - codes2[i]);
    }
    return code + 100 * lenDiff;

}

function naiveHammerDistance(str1, str2) {
    var dist = 0;

    str1 = str1.toLowerCase();
    str2 = str2.toLowerCase();

    for (var i = 0; i < str1.length; i++) {

        if (str2[i] && str2[i] !== str1[i]) {
            dist += Math.abs(str1.charCodeAt(i) - str2.charCodeAt(i)) + Math.abs(str2.indexOf(str1[i])) * 2;
        } else if (!str2[i]) {
            //  If there's no letter in the comparing string
            dist += dist;
        }
    }

    return dist;
}


// https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance
function levenshteinDistance(s, t) {
    if (s.length === 0) return t.length;
    if (t.length === 0) return s.length;

    return Math.min(
        levenshteinDistance(s.substr(1), t) + 1,
        levenshteinDistance(t.substr(1), s) + 1,
        levenshteinDistance(s.substr(1), t.substr(1)) + (s[0] !== t[0] ? 1 : 0)
    );
}

// https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance
// Compute the edit distance between the two given strings
function getEditDistance(a, b) {
    if (a.length === 0) return b.length;
    if (b.length === 0) return a.length;

    var matrix = [];

    // increment along the first column of each row
    var i;
    for (i = 0; i <= b.length; i++) {
        matrix[i] = [i];
    }

    // increment each column in the first row
    var j;
    for (j = 0; j <= a.length; j++) {
        matrix[0][j] = j;
    }

    // Fill in the rest of the matrix
    for (i = 1; i <= b.length; i++) {
        for (j = 1; j <= a.length; j++) {
            if (b.charAt(i - 1) == a.charAt(j - 1)) {
                matrix[i][j] = matrix[i - 1][j - 1];
            } else {
                matrix[i][j] = Math.min(matrix[i - 1][j - 1] + 1, // substitution
                    Math.min(matrix[i][j - 1] + 1, // insertion
                        matrix[i - 1][j] + 1)); // deletion
            }
        }
    }

    return matrix[b.length][a.length];
};



// module.exports._K_ = _K_;
// module.exports._branchDistance_ = _branchDistance_;
// module.exports._trace_ = _trace_;
module.exports.abs = abs;
module.exports.absZero = absZero;
module.exports.absNegZero = absNegZero;
