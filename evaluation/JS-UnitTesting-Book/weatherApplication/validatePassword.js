function validatePassword(passwordMessageID, passwordFieldID) {

    var passwordRegex = /((?=.*\d)(?=.*[a-z])(?=.*[A-Z])(?=.*[@#$%]).{6,20})/;
    var password = document.getElementById(passwordFieldID).value;

    if (!(passwordRegex.test(password) && password.length >= 6)) {
        document.getElementById(passwordMessageID).innerHTML = "(format is invalid)";

        return false;
    }

    return true;
};