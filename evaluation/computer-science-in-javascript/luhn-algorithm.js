/**
 * Uses Luhn algorithm to validate a numeric identifier.
 * @param {String} identifier The identifier to validate.
 * @return {Boolean} True if the identifier is valid, false if not.
 */
function isValidIdentifier(identifier) {
     
    var sum     = 0,
        alt     = false,
        i       = identifier.length-1,
        num;

    while (i >= 0){
            
        //get the next digit
        num = parseInt(identifier.charAt(i), 10);

        //if it's not a valid number, abort
        if (isNaN(num)){
            return false;
        }

        //if it's an alternate number...
        if (alt) {
            num *= 2;
            if (num > 9){
                num = (num % 10) + 1;
            }
        } 
        
        //flip the alternate bit
        alt = !alt;
        
        //add to the rest of the sum
        sum += num;

        //go to next digit
        i--;
    }

    //determine if it's valid
    return (sum % 10 == 0);
}