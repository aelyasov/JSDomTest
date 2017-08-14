module.exports = function(environment, logger) {

    (getElementsByTagNameCopy => {
	window.Document.prototype.getElementsByTagName = function(tag) {
	    //logger.debug("getElementsByTagName Document");
	    environment.tags.add(tag);
	    return Reflect.apply(getElementsByTagNameCopy, this, [tag]);
	};
    })(window.Document.prototype.getElementsByTagName);
    
    
    (getElementsByTagNameCopy => {
	window.Element.prototype.getElementsByTagName = function(tag) {
	    //logger.debug("getElementsByTagName Element");
	    environment.tags.add(tag);
	    return Reflect.apply(getElementsByTagNameCopy, this, [tag]);
	};
    })(window.Element.prototype.getElementsByTagName);
    

    (getAttributeCopy => {  
	window.Element.prototype.getAttribute = function(arg) {
	    //logger.debug("getAttribute");
	    let attrValue = Reflect.apply(getAttributeCopy, this, [arg]);
	    if (attrValue != null) {
		switch (arg) {
		case "class":
		    //logger.debug("getAttribute class");
		    environment.classes.add(attrValue);
        	    break;
		case "id":
		    //logger.debug("getAttribute id");
		    environment.ids.add(attrValue);
		    break; 
		}
	    }
	    return attrValue;
	};
    })(window.Element.prototype.getAttribute);
    
    
    (getElementByIdCopy => {
	window.Document.prototype.getElementById = function(id) {
	    //logger.debug("getElementById");
	    environment.ids.add(id);
	    return Reflect.apply(getElementByIdCopy, this, [id]);
	};
    })(window.Document.prototype.getElementById);
    
};
