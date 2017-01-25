  function validateEmail()
  {
  var x=document.forms["frm"]["email"].value;
  var atpos=x.indexOf("@");
  var dotpos=x.lastIndexOf(".");
  if (atpos<1 || dotpos<atpos+2 || dotpos+2>=x.length)
    {
	document.getElementById("error_email").innerHTML="* Invalid email id";
    return false;
    }
  return true;
  }