  function validateNumber(input) {
      var number = /^[0-9]+$/;
      if (input.match(number)) {
          return true;
      } else {
          document.getElementById("error_phnum").innerHTML = "* " + field + " Enter Numbers Only";
          return false;
      }
  }

/*
 * Types:
 * input: string
 */