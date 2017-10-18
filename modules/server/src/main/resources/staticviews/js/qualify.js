function showQualify(node, prefix) {
 console.log("showQualify node: " + node);
 var iriRegexp = /^<(.*)>$/g;
 var matchIri = iriRegexp.exec(node);
 if (matchIri) {
   var rawNode = matchIri[1];
   for (var key in prefix) {
     if (rawNode.startsWith(prefix[key])) {
       var longNode = "<" + rawNode + ">";
       return "<abbr title=\"" + longNode + "\">" + key + ":" + rawNode.slice(prefix[key].length) + "</abbr>";
     }
   }
   return "&lt;" + rawNode + "&gt;" ;
 }
 if (node.match(/^[0-9\"\'\_]/)) return node;
 console.log("Unknown format for node: " + node);
 return node;
}
