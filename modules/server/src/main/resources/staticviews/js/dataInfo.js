var codeMirrorData ;

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
}

$(document).ready(function(){

function resetResult(result) {
    console.log("Reset result: " + JSON.stringify(result));
    $("#resultDiv").empty();
    $("#resultDiv").data("result", result);
}

function showResult(result) {
    result = $("#resultDiv").data("result");
    console.log("Show result: " + JSON.stringify(result));
    if(result) {
        console.log("Result.nodesPrefixMap: " + JSON.stringify(result.nodesPrefixMap));
        var nodesPrefixMap = result.nodesPrefixMap ;
        var tableHead = "<thead><tr>" +
                "<th data-sortable=\"true\">Name</th>" +
                "<th data-sortable=\"true\">Value</th>" +
                "</tr></thead>";
        var tableBody = '';
        tableBody += "<tr><td>Number of triples</td><td>" + result.statements + "</td></tr>";
        $("#resultDiv").append("<table data-toggle=\"table\" data-sort-order=\"desc\" data-sort-name=\"node\">" +
                tableHead +
                tableBody +
                "</table>");
        var pre = $("<pre/>").text(JSON.stringify(result,undefined,2));
        var details = $("<details/>").append(pre);
        $("#resultDiv").append(details);
    }
}

function getDataFormat(element) {
    var format = element.options[element.selectedIndex].value;
    window.alert("Data format of " + element + " format: " + format);
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

 $('.dataPanel a[data-toggle="tab"]').on('shown.bs.tab', function (e) {
   var name = $(e.target).attr("href");
   console.log("New tab: " + name); // newly activated tab
   $('#rdfDataActiveTab').val(name);
 })


 $("#permalink").click(function(e) {
  e.preventDefault();
  console.log("click on permalink...");
  var data = codeMirrorData.getValue();
  var dataFormat = $("#dataFormat").find(":selected").text();
  var inference = $("#inference").find(":selected").text();
  var location = "/dataInfo?" +
      "data=" + encodeURIComponent(data) +
      "&dataFormat=" + encodeURIComponent(dataFormat) +
      "&inference=" + encodeURIComponent(inference) ;
    var href = urlShaclex + location
    console.log("NewHRef: " + href)
    window.location.assign(href) ;
  });

});
