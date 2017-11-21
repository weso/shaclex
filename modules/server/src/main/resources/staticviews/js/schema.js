var codeMirrorData ;
var codeMirrorSchema ;
var codeMirrorShapeMap ;
var codeMirrorTargetData ;

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
    codeMirrorSchema.setOption("theme",theme);
    codeMirrorShapeMap.setOption("theme",theme);
}

function hideShowSchema(show) {
   if (show) {
        $("#schemaDiv").hide();
    } else {
        $("#schemaDiv").show();
    }
}

function changeSchemaEmbedded(cb) {
    console.log("changeSchemaEmbedded: " + cb);
    console.log(cb);
    hideShowSchema(cb.checked);
}

function changeTriggerMode(value) {
    if (value) {
        console.log("Changing triggermode: " + value);
        switch (value.toUpperCase()) {
            case "TARGETDECLS":
//    $("#nodeShapeContainer").hide();
                $("#shapeMapDiv").hide();
//    console.log("Hiding all: " + value);
                break;
            /*  case "NODESHAPE":
                $("#nodeShapeContainer").show();
                console.log("Showing all: " + value);
                break;
              case "NODESTART":
                $("#nodeShapeContainer").show();
                console.log("Showing node only: " + value);
                break; */
            case "SHAPEMAP":
//    $("#nodeShapeContainer").show();
                $("#shapeMapDiv").show();
                console.log("Showing shape map: " + value);
                break;

        }
    }
}


$(document).ready(function(){


function getHost() {
    var port = window.location.port;
    return window.location.protocol + "//" +
           window.location.hostname + (port? ":" + port: "") ;
}

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
        var shapesPrefixMap = result.shapesPrefixMap ;
        console.log("nodesPrefixMap: " + JSON.stringify(nodesPrefixMap));
        console.log("shapesPrefixMap: " + JSON.stringify(shapesPrefixMap));
        if (result.isValid || result.valid) {
            $("#resultDiv").removeClass("notValid").addClass("valid");
            showShapeMap(result.shapeMap,nodesPrefixMap,shapesPrefixMap);
        } else {
            $("#resultDiv").removeClass("valid").addClass("notValid");
            $("#resultDiv").append($("<p>").text(result.message));
        }
        var pre = $("<pre/>").text(JSON.stringify(result,undefined,2));
        var details = $("<details/>").append(pre);
        $("#resultDiv").append(details);
    }
}

function showShape(status,shape,shapesPrefixMap) {
    var shapeClass;
    var textHasShape = '';
    if (status == 'conformant') {
      shapeClass = "hasShape success table-success"
      textHasShape = '+';
    }
    else {
     shapeClass = "hasNoShape danger table-danger";
     textHasShape = '-';
    }
    return "<td class=\"" + shapeClass + "\">" +
        "<code>" +
         textHasShape + showQualify(shape, shapesPrefixMap) +
        "</code></td>" ;
}

function showShapeMap(shapeMap,nodesPrefixMap, shapesPrefixMap) {
    var tableHead = "<thead><tr>" +
        "<th data-sortable=\"true\">Node</th>" +
        "<th data-sortable=\"true\">Shape</th>" +
        "<th>Evidences</th> " +
        "</tr></thead>";

    var tableBody = '';
    $.each(shapeMap, function(i) {
       var row = shapeMap[i];
       console.log("Association: " + JSON.stringify(shapeMap[i]) );
       tableBody += "<tr><td class='node'><code>" + showQualify(row.node, nodesPrefixMap) + "</code></td>" +
            showShape(row.status, row.shape, shapesPrefixMap) +
            "<td class='explanation'>" + escapeHtml(row.reason) + "</td></tr>" ;
     });
    $("#resultDiv").append("<table data-toggle=\"table\" data-sort-order=\"desc\" data-sort-name=\"node\">" +
        tableHead +
        tableBody +
        "</table>");
}

function showErrors(errors) {
    if (errors.length > 0) {
        var table = "";
        table += ""
        console.log("Errors" + JSON.stringify(errors));
        $.each(errors, function(i,e) {
            console.log("Error" + JSON.stringify(e));
            table += "<tr><td><pre>" + escapeHtml(e.error) + "</pre></td></tr>";
        });
        $("#resultDiv").append("<h2>Errors</h2><table>" + table + "</table>");
    }
}

function getDataFormat(element) {
    var format = element.options[element.selectedIndex].value;
    window.alert("Data format of " + element + " format: " + format);
}

/*
function prepareShapeMap() {
 console.log("Preparing shape map:" );
 inputRows = getInputRows();
 console.log("codeMirrorNodes:" + codeMirrorNodes.length + " inputRows: " + inputRows);
 var pairs = [];
 for (i=0; i< inputRows; i++) {
   var node = codeMirrorNodes[i].getValue();
   var shape = codeMirrorShapes[i].getValue();
   pairs.push(encodeURIComponent(node) + "@" + encodeURIComponent(shape));
   console.log("Current pairs: " + JSON.stringify(pairs));
 }
 var str = pairs.join(",");
 console.log("pairs: " + JSON.stringify(pairs) + ". Shape-map = " + str);
 return str;
}
*/

  var urlShaclex = getHost();
  console.log("urlShaclex: " + urlShaclex);

    // When loading document get result from data-result attribute and show it
    var result = $("#resultDiv").data("result");
    showResult(result);

//   $("#permalink").prop("href",window.location);

   /*$("#schemaEmbedded").change(function() {
       changeSchemaEmbedded(this.checked);
   }); */

    var schemaEmbeddedValue = $("#schemaEmbedded").is(":checked");
    console.log("Main...schemaEmbedded = " + schemaEmbeddedValue);
    hideShowSchema(schemaEmbeddedValue);


    var triggerModeValue = $("#triggerMode").val();
    console.log("Trigger mode = " + triggerModeValue);
    changeTriggerMode(triggerModeValue);

    var rdfData = document.getElementById("rdfData");
    if (rdfData) {
        codeMirrorData = CodeMirror.fromTextArea(rdfData, {
            lineNumbers: true,
            mode: "turtle",
            viewportMargin: Infinity,
            matchBrackets: true,
        });
    }

    var targetDataArea = document.getElementById("targetDataArea");
    if (targetDataArea) {
      codeMirrorTargetData = CodeMirror.fromTextArea(targetDataArea, {
      lineNumbers: true,
      mode: "turtle",
      viewportMargin: Infinity,
       matchBrackets: true,
      });
    }

    var schema = document.getElementById("schema")
    if (schema) {
        codeMirrorSchema = CodeMirror.fromTextArea(schema, {
            lineNumbers: true,
            mode: "shex",
            viewportMargin: Infinity,
            matchBrackets: true
        });
    }

    var shapeMap = document.getElementById("shapeMap")
    if (shapeMap) {
        codeMirrorShapeMap = CodeMirror.fromTextArea(shapeMap, {
            lineNumbers: true,
            mode: "shex",
            viewportMargin: Infinity,
            matchBrackets: true
        });
        codeMirrorShapeMap.setSize(null,"5em");
    }

 $('.schemaPanel a[data-toggle="tab"]').on('shown.bs.tab', function (e) {
   var name = $(e.target).attr("href");
   console.log("New schema tab: " + name);
   $('#activeSchemaTab').val(name);
 })

 $("#permalink").click(function(e) {
  e.preventDefault();
  console.log("click on permalink...");
  var data = codeMirrorData.getValue();
  var schema = codeMirrorSchema.getValue();
  var dataFormat = $("#dataFormat").find(":selected").text();
  var schemaFormat = $("#schemaFormat").find(":selected").text();
  var schemaEngine = $("#schemaEngine").find(":selected").text();
  var triggerMode = $("#triggerMode").find(":selected").text();
  var inference = $("#inference").find(":selected").text();
  var shapeMap = codeMirrorShapeMap.getValue(); // prepareShapeMap();
  var schemaEmbedded = $("#schemaEmbedded").is(":checked");
  if (schemaEmbedded) {  schema = ""; }
    console.log("Trigger mode in AJAX query:" + triggerMode);
    var location = "/validate?" +
      "data=" + encodeURIComponent(data) +
      "&dataFormat=" + encodeURIComponent(dataFormat) +
      "&schema=" + encodeURIComponent(schema) +
      "&schemaFormat=" + encodeURIComponent(schemaFormat) +
      "&schemaEngine=" + encodeURIComponent(schemaEngine) +
      "&triggerMode=" + encodeURIComponent(triggerMode) +
      "&schemaEmbedded=" + encodeURIComponent(schemaEmbedded) +
      "&inference=" + encodeURIComponent(inference) +
      "&shapeMap=" + encodeURIComponent(shapeMap);
    var href = urlShaclex + location
    console.log("NewHRef: " + href)
    window.location.assign(href) ;
  });

});