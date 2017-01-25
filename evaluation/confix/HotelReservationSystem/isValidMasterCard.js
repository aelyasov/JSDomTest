function isValidMasterCard(cardNumber){
  // if you want to accept Mastercard.
  if(cardNumber.charAt(0) == '5' && (cardNumber.charAt(1) == '1' || cardNumber.charAt(1) == '5') && cardNumber.length == 16){
    return isValidCard(cardNumber);
  }
  return false;
}