function validateEmptyFields(passwordMessageID, userNameMessageID, passwordFieldID, userNameFieldID) {

    document.getElementById(passwordMessageID).innerHTML = "";
    document.getElementById(userNameMessageID).innerHTML = "";

    if (!document.getElementById(userNameFieldID).value) {
        document.getElementById(userNameMessageID).innerHTML = "(field is required)";

        return false;
    }

    if (!document.getElementById(passwordFieldID).value) {
        document.getElementById(passwordMessageID).innerHTML = "(field is required)";

        return false;
    }

    return true;
};

/*
 * Types:
 * passwordMessageID: string
 * userNameMessageID: string 
 * passwordFieldID: string 
 * userNameFieldID: string
 */
