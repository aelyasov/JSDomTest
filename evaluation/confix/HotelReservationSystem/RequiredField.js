 function RequiredField(felements) {
     var i, field;
     for (i = 0; i < felements.length; i++) {
         field = felements[i];
         value = trim(felements[i]);

         if (value == "" || value == null) {
             document.getElementById("error_" + field).innerHTML = "* " + field + " Required";
             return false;
         }
     }
     return true;
 }

 function trim(s) {
     return s.replace(/^\s*/, "").replace(/\s*$/, "");
 }

/*
 * Types:
 * felements: [string]
 */