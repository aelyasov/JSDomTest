function isValidVISA(cardNumber){
  // if you want to accept Visa (Debit/Electron/Credit Card) ONLY!
  if(cardNumber.charAt(0) == '4' && (cardNumber.length == 13 || cardNumber.length == 16)){
    return isValidCard(cardNumber);
  }
  
  return false;
}