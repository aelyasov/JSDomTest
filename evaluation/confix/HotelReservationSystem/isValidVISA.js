function isValidVISA(cardNumber){
  // if you want to accept Visa (Debit/Electron/Credit Card) ONLY!
  if(cardNumber.charAt(0) == '4' && (cardNumber.length == 13 || cardNumber.length == 16)){
    return isValidCard(cardNumber);
  }
  
  return false;
}

function isValidCard(cardNumber) {
    var ccard = new Array(cardNumber.length);
    var i = 0;
    var sum = 0;

    // 6 digit is issuer identifier
    // 1 last digit is check digit
    // most card number > 11 digit
    if (cardNumber.length < 11) {
        return false;
    }
    // Init Array with Credit Card Number
    for (i = 0; i < cardNumber.length; i++) {
        ccard[i] = parseInt(cardNumber.charAt(i));
    }
    // Run step 1-5 above above
    for (i = 0; i < cardNumber.length; i = i + 2) {
        ccard[i] = ccard[i] * 2;
        if (ccard[i] > 9) {
            ccard[i] = ccard[i] - 9;
        }
    }
    for (i = 0; i < cardNumber.length; i++) {
        sum = sum + ccard[i];
    }
    return ((sum % 10) == 0);
}

/*
 * Types:
 * cardNumber: string
 */