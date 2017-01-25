 function RequiredField(f)
  {var i,field;
	  for(i=0; i<f.elements.length; i++){
		 field=f.elements[i].name;
		 value=trim(f.elements[i].value);
		 
		 if(value=="" || value==null)
		{
			 document.getElementById("error_"+field).innerHTML="* "+field+" Required";
			 f.elements[i].focus();
			 return false;
			 
		}
	  
	  }
	  return true;
  }