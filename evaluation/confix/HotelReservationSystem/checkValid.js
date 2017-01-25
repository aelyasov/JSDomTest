function checkValid(f){
  var cardNumber=f.cardno.value;
  
  if(document.getElementById('c1').checked) {
    if(!isValidVISA(cardNumber)){
      document.getElementById("error_cardno").innerHTML="Invalid VISA Card No";
      return false;
    }
    
  }
  
  if(document.getElementById('c2').checked) {
    
    if(!isValidMasterCard(cardNumber))
    {		  
      document.getElementById("error_cardno").innerHTML="Invalid MasterCard No";
      return false;
    }
  }
  
  return true;
}	