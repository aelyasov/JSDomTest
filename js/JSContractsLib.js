this.JSContractsLib = (function JSContractsLib() {
  return {
    countDomElements: function(doc) {
      return doc.getElementsByTagName('*').length;
    }
  }
}()
);