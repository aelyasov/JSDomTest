/*t dom */
function test() {
    var pass = document.getElementById("pass").value;
    var c_pass = document.getElementById("c_pass").value;
    if (pass != c_pass) {
        document.getElementById("pass").value = "";
        document.getElementById("c_pass").value = "";
        document.getElementById("error_Password").innerHTML = "Passwords do not match";
        return false;
    }
    return true;
}