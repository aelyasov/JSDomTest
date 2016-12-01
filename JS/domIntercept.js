module.exports = function(document, environment) {
    
    (getElementsByTagNameCopy => {
	document.getElementsByTagName = tag => {
	    environment.tags.add(tag);
	    return Reflect.apply(getElementsByTagNameCopy, document, [tag]);
	};
    })(document.getElementsByTagName);
    
    
    (getElementsByTagNameCopy => {
	document.defaultView.Element.prototype.getElementsByTagName = function(tag) {
            environment.tags.add(tag);
            return Reflect.apply(getElementsByTagNameCopy, this, [tag]);
        };
    })(document.defaultView.Element.prototype.getElementsByTagName);
    
    
    (getAttributeCopy => {  
	document.defaultView.Element.prototype.getAttribute = function(arg) {
	    let attrValue = Reflect.apply(getAttributeCopy, this, [arg]);
	    if (attrValue != null) {
		switch (arg) {
		case "class":
		    environment.classes.add(attrValue);
        	    break;
		case "id":
		    environment.ids.add(attrValue);
		    break; 
		}
	    }
	    return Reflect.apply(getAttributeCopy, this, [arg]);
	};
    })(document.defaultView.Element.prototype.getAttribute);
    
    
    (getElementByIdCopy => {
	document.getElementById = id => {
            environment.ids.add(id);
	    return Reflect.apply(getElementByIdCopy, document, [id]);
	};
    })(document.getElementById);
};
