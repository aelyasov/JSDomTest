function validateUserName(userNameMessageID, userNameFieldID) {

    var userNameRegex = /^[_A-Za-z0-9-]+(\.[_A-Za-z0-9-]+)*@[A-Za-z0-9]+(\.[A-Za-z0-9]+)*(\.[A-Za-z]{2,})$/;
    var userName = document.getElementById(userNameFieldID).value;

    if (!userNameRegex.test(userName)) {
        document.getElementById(userNameMessageID).innerHTML = "(format is invalid)";

        return false;
    }

    return true;
};