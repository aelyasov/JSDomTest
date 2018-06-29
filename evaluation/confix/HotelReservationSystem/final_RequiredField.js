/*t dom:[string] */
function test(felements) {
    var field, value;
    for (var i = 0; i < felements.length; i++) {
        field = felements[i];
        value = trim(felements[i]);
        if (value == "" || value == null) {
            document.getElementById("error_" + field).innerHTML = "* " + field + " Required";
            return false;
        }
    }
    return true;
}

function trim(s) {
    return s.replace(/^\s*/, "").replace(/\s*$/, "");
}
