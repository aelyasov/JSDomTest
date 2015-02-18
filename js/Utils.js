this.JSDomTestUtils = (function JSDomTestUtils() {
    var privateVar;

    function privateFunction() {};

    function _enumUpToN(n) {
        var res = [];
        for (var i = 0; i < n; i++) {
            res.push(i);
        }
        return res;
    };

    function _combinations(arr, N) {
        var result = [];
        var f = function(prefix, arr) {
            if (prefix.length < N) {
                for (var i = 0; i < arr.length; i++) {
                    result.push(prefix.concat([arr[i]]));
                    f(prefix.concat([arr[i]]), arr.slice(i + 1));
                }
            }
        }
        f([], arr);
        return result;
    };

    function _mkJQSelector(arrIds) {
        var idToSel = function(i) {
            return "#" + i;
        }
        return arrIds.map(idToSel).join(",");
    };

    // assign unique ids to all elements of the body
    function _assignUniqueIdsToDOM(doc) {
        var all = doc.body.getElementsByTagName("*");
        for (var i = 0, max = all.length; i < max; i++) {
            all[i].setAttribute("id", i);
        };
        return all.length;
    }

    return {
        getAllIdPairs: function(doc, N) {
            var tagsN = _assignUniqueIdsToDOM(doc);
            console.log("tagsN: " + tagsN);
            console.log("console.log: " + _enumUpToN(tagsN));
            return _combinations(_enumUpToN(tagsN), N);
        },

        mkFunTarget: function(doc, ids) {
            console.log(_mkJQSelector(ids));
            return _mkJQSelector(ids);
        }

    }
}());
