  function validateNumber(f)
  {
	  var number = /^[0-9]+$/;
	  input =f.phnum.value;
	  if(input.match(number))
		  {
		  	return true;
		  }
	  else
		  {
		  	document.getElementById("error_phnum").innerHTML="* "+field+" Enter Numbers Only";
		  	return false;
		  }
  }