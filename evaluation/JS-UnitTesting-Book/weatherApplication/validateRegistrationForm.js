function validateRegistrationForm(userNameMessage, passwordMessage1, userNameField, passwordField1, passwordField2) {
    var password1 = document.getElementById(passwordField1).value;
    var password2 = document.getElementById(passwordField2).value;

    // Empty messages ...
    document.getElementById(userNameMessage).innerHTML = "";
    document.getElementById(passwordMessage1).innerHTML = "";

    // create the loginClient object in order to validate fields ...
    var loginClient = new weatherapp.LoginClient();

    var loginForm = {};

    loginForm.userNameField = userNameField;
    loginForm.userNameMessage = userNameMessage;
    loginForm.passwordField = passwordField1;
    loginForm.passwordMessage = passwordMessage1;

    // validate empty username and password fields.
    if (!loginClient.validateEmptyFields(loginForm)) {
        return false;
    }

    // validate that password fields have the same value ...
    if (password1 != password2) {
        document.getElementById(passwordMessage1).innerHTML = "(Passwords must be identical)";

        return false;
    }

    // check if the username is correct ...	
    if (!loginClient.validateUserName(loginForm)) {
        document.getElementById(userNameMessage).innerHTML = "(format is invalid)";

        return false;
    }

    // check if the password is correct ...
    if (!loginClient.validatePassword(loginForm)) {
        document.getElementById(passwordMessage1).innerHTML = "(format is invalid)";

        return false;
    }

    return true;
};