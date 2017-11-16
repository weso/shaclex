function getHost() {
    var port = window.location.port;
    return window.location.protocol + "//" +
           window.location.hostname + (port? ":" + port: "") ;
}


function showQualify(node, prefix) {
 console.log("showQualify node)");
 console.log(node);
 var iriRegexp = /^<(.*)>$/g;
 var matchIri = iriRegexp.exec(node);
 if (matchIri) {
   var rawNode = matchIri[1];
   for (var key in prefix) {
     if (rawNode.startsWith(prefix[key])) {
       var localName = rawNode.slice(prefix[key].length);
        console.log("qualifying " + localName)
/*       if (localName.indexOf("/") > -1) {
        return "&lt;" + rawNode + "&gt;" ;
       } else */
        var longNode = "<" + rawNode + ">";
        return "<abbr title=\"" + longNode + "\">" + key + ":" + localName + "</abbr>";
     }
   }
   return "&lt;" + rawNode + "&gt;" ;
 }
 if (node.match(/^[0-9\"\'\_]/)) return node;
 console.log("Unknown format for node: " + node);
 return node;
}

function resetResult(result) {
    console.log("Reset result: " + JSON.stringify(result));
    $("#resultDiv").empty();
    $("#resultDiv").data("result", result);
}

var entityMap = {
    '&': '&amp;',
    '<': '&lt;',
    '>': '&gt;',
    '"': '&quot;',
    "'": '&#39;',
    '/': '&#x2F;',
    '`': '&#x60;',
    '=': '&#x3D;',
    '\n': '<br/>'
};

function escapeHtml (string) {
    return String(string).replace(/[&<>"'`=\/\n]/g, function (s) {
        return entityMap[s];
    });
}

