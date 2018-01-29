var codeMirrorData ;
var codeMirrorQuery ;

function changeMode(element,syntax) {
    var mode = "turtle";
    switch (syntax.toUpperCase()) {
        case "TURTLE": mode = "turtle" ;
            break ;
        case "N-TRIPLES": mode = "turtle" ;
            break ;
        case "RDF/XML": mode = "xml" ;
            break ;
        case "TRIX": mode = "xml" ;
            break ;
        case "SHEXJ" : mode = "javascript" ;
            break ;
        case "RDF/JSON" : mode = "javascript" ;
            break ;
        case "JSON-LD" : mode = "javascript" ;
            break ;
        case "SHEXC": mode = "shex" ;
            break ;
    }
    element.setOption("mode",mode);
}

function changeTheme(theme) {
    codeMirrorData.setOption("theme",theme);
    codeMirrorQuery.setOption("theme",theme);
}

$(document).ready(function(){

function resetResult(result) {
    console.log("Reset result: " + JSON.stringify(result));
    $("#resultDiv").empty();
    $("#resultDiv").data("result", result);
}

function showNodeQualified(node, prefix) {
 console.log("showQualify node)");
 console.log(node);
 if (node.type=="uri") {
   var rawNode = node.value;
   for (var key in prefix) {
      if (rawNode.startsWith(prefix[key])) {
        var longNode = "<" + rawNode + ">";
        return "<abbr title=\"" + longNode + "\">" + key + ":" + rawNode.slice(prefix[key].length) + "</abbr>";
      }
    }
    return "&lt;" + rawNode + "&gt;" ;
 } else {
   return node.value
 }
}

function showResult(result,nodesPrefixMap) {
  if (result) {
    var tableHead = '<thead>';
    var vars = result.head.vars;
    console.log(vars);
    $.each(vars, function(i) {
        tableHead += "<th data-sortable=\"true\">" + vars[i] + "</th>";
    });
    tableHead += '</thead>';

    var tableBody = '';
    $.each(result.results.bindings, function(i) {
       var row = result.results.bindings[i];
       console.log("Binding: " + JSON.stringify(row) );
       tableBody += "<tr>";
       $.each(vars,function(i) {
         tableBody += "<td><code>";
         if (row[vars[i]]) tableBody += showNodeQualified(row[vars[i]], nodesPrefixMap);
         tableBody += "</code></td>";
       });
       tableBody += "</tr>" ;
     });
    $("#resultDiv").append("<table data-toggle=\"table\" data-sort-order=\"desc\" data-sort-name=\"node\">" +
        tableHead +
        tableBody +
        "</table>");
     var pre = $("<pre/>").text(JSON.stringify(result,undefined,2));
     var details = $("<details/>").append(pre);
     $("#resultDiv").append(details);
    }
    else {
      console.log("Result: ")
      console.log(result);
    }
}

 var urlShaclex = getHost();
 console.log("urlShaclex: " + urlShaclex);

// When loading document get result from data-result attribute and show it
 var result = $("#resultDiv").data("result");
 showResult(result);

 var rdfData = document.getElementById("rdfData");
 if (rdfData) {
    codeMirrorData = CodeMirror.fromTextArea(rdfData, {
            lineNumbers: true,
            mode: "turtle",
            viewportMargin: Infinity,
            matchBrackets: true,
    });
 }

 console.log("Formatting query")
 var query = document.getElementById("queryData")
 if (query) {
    codeMirrorQuery = CodeMirror.fromTextArea(query, {
            lineNumbers: true,
            mode: "sparql",
            viewportMargin: Infinity,
            matchBrackets: true
        });
    }

 /** The following lines associate events to the panel tabs, when a user clicks on a panel,
   *  the corresponding xxxActiveTab is changed
     *  dataPanel2, schemaPanel, shapeMapPanel are the classes of the panels
     *  dataPanel2 is because it appears in 2 panels (validation requires 2)
     */
 $('.dataPanel2 a[data-toggle="tab"]').on('shown.bs.tab', function (e) {
   var name = $(e.target).attr("href");
   console.log("New data tab: " + name);
   $('#rdfDataActiveTab').val(name);
 });

 $('.queryPanel a[data-toggle="tab"]').on('shown.bs.tab', function (e) {
   var name = $(e.target).attr("href");
   console.log("New Query tab: " + name);
   $('#activeQueryTab').val(name);
 });

 $("#permalink").click(function(e) {
  e.preventDefault();
  console.log("click on permalink...");
  var data = codeMirrorData.getValue();
  var query = codeMirrorQuery.getValue();
  var dataFormat = $("#dataFormat").find(":selected").text();
  var inference = $("#inference").find(":selected").text();
  var location = "/query?" +
      "data=" + encodeURIComponent(data) +
      "&dataFormat=" + encodeURIComponent(dataFormat) +
      "&query=" + encodeURIComponent(query) +
      "&inference=" + encodeURIComponent(inference);
    var href = urlShaclex + location
    console.log("NewHRef: " + href)
    window.location.assign(href) ;
  });

});
