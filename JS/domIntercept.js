module.exports = function(document, environment) {
    
    (getElementsByTagNameCopy => {
	document.getElementsByTagName = tag => {
	    environment.tags.push(tag);
	    return Reflect.apply(getElementsByTagNameCopy, document, [tag]);
	};
    })(document.getElementsByTagName);
    
    
    (getElementsByTagNameCopy => {
	document.defaultView.Element.prototype.getElementsByTagName = function(tag) {
            environment.tags.push(tag);
            return Reflect.apply(getElementsByTagNameCopy, this, [tag]);
        };
    })(document.defaultView.Element.prototype.getElementsByTagName);
    
    
    (getAttributeCopy => {  
	document.defaultView.Element.prototype.getAttribute = function(arg) {
	    switch (arg) {
            case "class":
		environment.classes.push(Reflect.apply(getAttributeCopy, this, [arg]));
        	break;
            case "id": environment.ids.push(Reflect.apply(getAttributeCopy, this, [arg]));
		break; 
            }
	    return Reflect.apply(getAttributeCopy, this, [arg]);
	};
    })(document.defaultView.Element.prototype.getAttribute);
    
    
    (getElementByIdCopy => {
	document.getElementById = id => {
        	    environment.ids.push(id);
	    return Reflect.apply(getElementByIdCopy, document, [id]);
	};
    })(document.getElementById);
};
