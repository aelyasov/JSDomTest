function appendChild_(parentId, childId){
  var parentNd = document.getElementById(parentId);
  // console.log(parentNode.rooted);
  var childNd = document.getElementById(childId);
  if (isAncestor(parentNd, childNd)) HierarchyRequestError();
  if (childNd.nodeType === Node.DOCUMENT_NODE) HierarchyRequestError();
  ensureSameDoc(parentNd,childNd);
  insert(childNd, parentNd, parentNd.childNodes.length);
  return childNd;
};

// NodeList.prototype.splice = Array.splice;

function insert(child, parent, index){
  var kids = parent.childNodes;

  // If we are already a child of the specified parent, then t
  // the index may have to be adjusted.
  if (child.parentNode === parent) {
    var currentIndex = childIndex(child);
    // If we're not moving the node, we're done now
    // XXX: or do DOM mutation events still have to be fired?
    if (currentIndex === index) return;
    // If the child is before the spot it is to be inserted at,
    // then when it is removed, the index of that spot will be
    // reduced.
    if (currentIndex < index) index--;
  }

  // Special case for document fragments
  // XXX: it is not at all clear that I'm handling this correctly.
  // Scripts should never get to see partially
  // inserted fragments, I think.  See:
  // http://lists.w3.org/Archives/Public/www-dom/2011OctDec/0130.html
  if (child.nodeType === Node.DOCUMENT_FRAGMENT_NODE) {
    var  c;
    while(c = child.firstChild)
      insert(c, parent, index++);
    return;
  }

  // parent.appendChild(child);

  // If both the child and the parent are rooted, then we want to
  // transplant the child without uprooting and rerooting it.
  // *** start comment
  // // if (child.rooted && parent.rooted) {
  //   // Remove the child from its current position in the tree
  //   // without calling remove(), since we don't want to uproot it.
  //   var curpar = child.parentNode, curidx = childIndex(child);
  //   turnObjToArray(child.parentNode.childNodes).splice(childIndex(child), 1);
  //   //curpar.modify();

  //   // And insert it as a child of its new parent
  //   child.parentNode = parent;
  //   turnObjToArray(kids).splice(index, 0, child);
  //   // child._index = index;              // Optimization
  //   // parent.modify();

  //   // Generate a move mutation event
  //   // parent.ownerDocument.mutateMove(child);
  // // } else {
  // *** end comment
    // If the child already has a parent, it needs to be
    // removed from that parent, which may also uproot it
    if (child.parentNode) child.parentNode.removeChild(child);

    // Now insert the child into the parent's array of children
    child.parentNode = parent;
    Array.prototype.slice.call(kids, index, 0, child);
    // splice(kids, index, 0, child);
    //Array.from(kids).slice(kids, index, 0, child);
    // parent.modify();
    // child._index = index;              // Optimization

    // And root the child if necessary
    // if (parent.rooted) parent.ownerDocument.mutateInsert(child);
  // // }

  // // Script tags use this hook
  // // if (parent._addchildhook) parent._addchildhook(this);
}

function isAncestor(parentNd, childNd){
  // If they belong to different documents, then they're unrelated.
  if (parentNd.ownerDocument != childNd.ownerDocument) return false;
  // If one is rooted and one isn't then they're not related
  // check equality of roots. it seems to be necessary when
  // the documents has frames or DocumentFragments
  // if (parentNode.rooted !== parentNode.rooted) return false;
  // Otherwise check by traversing the parentNode chain
  for(var e = parentNd; e; e = e.parentNode) {
    if (e === childNd) return true;
  }
  return false;
};

function HierarchyRequestError() { throw DOMException(HIERARCHY_REQUEST_ERR); }
function WrongDocumentError() { throw DOMException(WRONG_DOCUMENT_ERR); }
function NotSupportedError() { throw DOMException(NOT_SUPPORTED_ERR); }

function ensureSameDoc(parentNd,childNd){
  // Get the owner of the node, the node itself, if it is a document
  var ownerdoc = parentNd.ownerDocument || parentNd;

  if (childNd.nodeType === Node.DOCUMENT_TYPE_NODE) {
    if (childNd.ownerDocument !== null && childNd.ownerDocument !== ownerdoc)
      WrongDocumentError();
     childNd.ownerDocument = ownerdoc;
  } else {
    // The spec's algorithm says to call adoptNode
    // unconditionally, which will remove it from its current
    // location in the document even it if is not changing
    // documents.  I don't do that here because that would cause a
    // lot of unnecessary uproot and reroot mutation events.
    if (childNd.ownerDocument !== ownerdoc)
      adoptNode(ownerdoc, childNd);
  }
  // XXX: this step does not seem necessary.
  // If mutation events are added, however, it becomes necessary
  if (childNd.ownerDocument !== ownerdoc) HierarchyRequestError();

};


function adoptNode(ownerdoc,childNd){
  if (childNd.nodeType === Node.DOCUMENT_NODE ||
      childNd.nodeType === Node.DOCUMENT_TYPE_NODE) NotSupportedError();

  if (childNd.parentNode) childNd.parentNode.removeChild(node)

  if (childNd.ownerDocument !== ownerdoc)
    recursivelySetOwner(childNd, ownerdoc);

  return node;
};


function recursivelySetOwner(node, owner) {
  node.ownerDocument = owner;
  var kids = node.childNodes;
  for(var i = 0, n = kids.length; i < n; i++)
    recursivelySetOwner(kids[i], owner);
}

function childIndex(child){
  var i = 0;
  while( (child = child.previousSibling) != null )
    i++;
  return i;
}

var turnObjToArray = function(obj) {
  return [].map.call(obj, function(element) {
    return element;
  })
};
