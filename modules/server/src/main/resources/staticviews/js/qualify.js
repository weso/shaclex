var prefix = 
 {"":"http://example.org/",
  "schema":"http://schema.org/",
  "xsd":"http://www.w3.org/2001/XMLSchema#",
  "foaf":"http://xmlns.com/foaf/0.1/"
 };
 
var node = "http://schema.org/Person";

function showQualify(node, prefix) {
 if (node.match(/^[0-9\"\'\_]/)) return node;
 for (var key in prefix) {
     if (node.startsWith(prefix[key])) {
         return key + ":" + node.slice(prefix[key].length);
     }
 }
 return "<" + node + ">" ;
}

console.log("Result: " + showQualify("http://schema.org/Person", prefix));
console.log("Result: " + showQualify("http://example.org/User", prefix));
console.log("Result: " + showQualify("http://other.example.org/User", prefix));
console.log("Result: " + showQualify("\"Hola\"", prefix));
console.log("Result: " + showQualify("23", prefix));
console.log("Result: " + showQualify("_:x", prefix));
