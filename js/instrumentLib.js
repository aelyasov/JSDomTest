// var _K_ = 1,
//     _branchDistance_ = [],
//     _trace_ = [1];

function abs(x, y) {
    var typeX = typeof(x);
    var typeY = typeof(y);
    if (typeX == typeY) {
	switch (typeX)
	{
	    case "number" : console.log("num"); return Math.abs(x-y); break;
	    case "string" : console.log("str"); return getEditDistance(x,y); break;
	    case "string" : return naiveHammerDistance(x,y);console.log("str"); break;
	    case "boolean": return Math.abs(x-y); break;
	    case "object" : return Number(x == y); break;
	    default: return 0;
	}
    } else {
	return 1;
    }
}


function absZero(x) {
    var typeX = typeof(x);
    switch (typeX) {
	default: return _K_;
    }
}


function absNegZero(x) {
    var typeX = typeof(x);
    switch (typeX) {
    case "number" : return Math.abs(x);
    case "string" : return Math.abs(x.length);
    default: return _K_;
    }
}


function naiveHammerDistance(str1, str2) {
    var dist = 0;
    
    str1 = str1.toLowerCase();
    str2 = str2.toLowerCase();

    for(var i = 0; i < str1.length; i++) {
	
        if(str2[i] && str2[i] !== str1[i]) {
            dist += Math.abs(str1.charCodeAt(i) - str2.charCodeAt(i)) + Math.abs(str2.indexOf( str1[i] )) * 2;
        } 
        else if(!str2[i]) {
            //  If there's no letter in the comparing string
            dist += dist;
        }
    }

    return dist;
}


// https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance
function levenshteinDistance (s, t) {
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
  if(a.length === 0) return b.length; 
  if(b.length === 0) return a.length; 

  var matrix = [];

  // increment along the first column of each row
  var i;
  for(i = 0; i <= b.length; i++){
    matrix[i] = [i];
  }

  // increment each column in the first row
  var j;
  for(j = 0; j <= a.length; j++){
    matrix[0][j] = j;
  }

  // Fill in the rest of the matrix
  for(i = 1; i <= b.length; i++){
    for(j = 1; j <= a.length; j++){
      if(b.charAt(i-1) == a.charAt(j-1)){
        matrix[i][j] = matrix[i-1][j-1];
      } else {
        matrix[i][j] = Math.min(matrix[i-1][j-1] + 1, // substitution
                                Math.min(matrix[i][j-1] + 1, // insertion
                                         matrix[i-1][j] + 1)); // deletion
      }
    }
  }

  return matrix[b.length][a.length];
};


function convertArg(type, arg) {
    switch(type) {
    case "JS_INT": return parseInt(arg); break;
    default: return arg;    
    }
}


// module.exports._K_ = _K_;
// module.exports._branchDistance_ = _branchDistance_;
// module.exports._trace_ = _trace_;
module.exports.abs = abs;
module.exports.absZero = absZero;
module.exports.absNegZero = absNegZero;
module.exports.convertArg = convertArg;
