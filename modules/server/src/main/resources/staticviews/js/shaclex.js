function getHost() {
 var port = window.location.port;
 return window.location.protocol + "//" + window.location.hostname + (port? ":" + port: "") ;
}

var urlShaclex = getHost();

console.log("urlShaclex: " + urlShaclex);

var codeMirrorData ;
var codeMirrorSchema ;
var codeMirrorNodes = new Array();
var codeMirrorShapes = new Array();
var inputRows = 0;

function newCodeMirrorNode(n) {
 var nodeId = document.getElementById("node" + n);
 if (nodeId) {
  var codeMirrorNode = CodeMirror.fromTextArea(nodeId, {
  lineNumbers: false,
   mode: "turtle",
   scrollbarStyle: "null",
   tabindex: 2*n,
   height: 1,
   extraKeys: { Tab: false }
  });
  codeMirrorNode.on("beforeChange", noNewLine);
  codeMirrorNode.setSize(null,"1.5em");
  codeMirrorNodes.push(codeMirrorNode);
 } else {
   console.log("Not found node" + n);
 }
}

function newCodeMirrorShape(n) {
 var shapeId = document.getElementById("shape" + n);
 if (shapeId) {
  var codeMirrorShape = CodeMirror.fromTextArea(shapeId, {
   lineNumbers: false,
   mode: "turtle",
   scrollbarStyle: "null",
   tabindex: 2*n+1,
   height: 1,
   extraKeys: { Tab: false }
  });
 codeMirrorShape.on("beforeChange", noNewLine);
 codeMirrorShape.setSize(null,"1.5em");
 console.log("Current codeMirrorShapes: " + codeMirrorShapes.length);
 codeMirrorShapes.push(codeMirrorShape);
 } else {
   console.log("Not found shape" + n);
 }
}

function getInputRows() {
 return $("#rowsCounter").data("value");
}

function setInputRows(n) {
 $("#rowsCounter").data("value",n);
}

function incrementInputRows() {
 inputRows = getInputRows();
 inputRows++
 setInputRows(inputRows);
 return inputRows;
}

function addNodeShapeEntry() {
  inputRows = incrementInputRows();
  var nodeEntry =  "<div id ='nodeDiv"+ inputRows + "' class='nodeDiv'><label>Node<textarea placeholder='Node' id='node" + inputRows + "' placeholder='Node...'></textarea></label></div>"
  var shapeEntry = "<div id ='shapeDiv"+inputRows  + "' class='shapeDiv'><label>Shape<textarea placeholder='Shape' id='shape" + inputRows + "' placeholder='Shape...'></textarea></label></div><div style='clear:both'></div>"
  console.log("Setting styles... on #nodeDiv" + inputRows);
  $("#nodeDiv"+inputRows).css("float","left");
  $("#nodeDiv"+inputRows).css("width","30%");
  $("#shapeDiv"+inputRows).css("float","right");
  $("#shapeDiv"+inputRows).css("width","30%");
  $("#nodeShapeContainer").append(nodeEntry);
  $("#nodeShapeContainer").append(shapeEntry);
  $("#nodeShapeContainer").append("<div style='clear:both'/>");
  newCodeMirrorNode(inputRows);
  newCodeMirrorShape(inputRows);
  console.log("Added row " + inputRows);
}

function removeNodeShapeEntry() {
  $inputRows = getInputRows();
  if (inputRows > 0) {
    console.log("Removing entry..."+ inputRows);
    $("#shapeDiv" + inputRows).remove();
    $("#nodeDiv" + inputRows).remove();
    codeMirrorNodes.pop();
    codeMirrorShapes.pop();
    inputRows--;
    setInputRows(inputRows);
  }
  console.log("Current rows" + inputRows + ". codeMirrorNodes: " + codeMirrorNodes + " codeMirrorShapes: " + codeMirrorShapes);
}

// Don't allow newline before change in CodeMirror
function noNewLine(instance,change) {
    var newtext = change.text.join("").replace(/\n/g, ""); // remove ALL \n !
    change.update(change.from, change.to, [newtext]);
    return true;
}

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
}

function changeSchemaEmbedded(value) {
 console.log("Changing schemaEmbedded: " + value);
 if (value==="schemaEmbedded") {
  $("#schemaDiv").hide();
 } else {
  $("#schemaDiv").show();
 }
}

function changeTriggerMode(value) {
 if (value) {
 console.log("Changing triggermode: " + value);
 switch (value.toUpperCase()) {
  case "TARGETDECLS":
    $("#nodeDiv").hide();
    $("#shapeDiv").hide();
    console.log("Hiding all: " + value);
    break;
  case "NODESHAPE":
    $("#nodeDiv").show();
    $("#shapeDiv").show();
    console.log("Showing all: " + value);
    break;
  case "NODESTART":
    $("#nodeDiv").show();
    $("#shapeDiv").hide();
    console.log("Showing node only: " + value);
    break;
  }
 }
}

function showResult(result) {
  console.log("Result: " + result);
  var validText;
  if (result.isValid) {
   $("#resultDiv").removeClass("notValid").addClass("valid");
   validText = "Valid";
  } else {
   $("#resultDiv").removeClass("valid").addClass("notValid");
   validText = "Not valid";
  }
  $("#resultDiv").empty();
  $("#resultDiv").append($("<h2>").text(validText));
  var pre = $("<pre/>").html(JSON.stringify(result,undefined,2));
  var details = $("<details/>").append(pre);
  $("#resultDiv").append(details);
}

function getDataFormat(element) {
 var format = element.options[element.selectedIndex].value;
 window.alert("Data format of " + element + " format: " + format);
}

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

$(document).ready(function(){

console.log("Main Url = " + urlShaclex);

var schemaEmbeddedValue = $("#toggleSchemaEmbedded").val();
console.log("Schema embedded = " + schemaEmbeddedValue);
changeSchemaEmbedded(schemaEmbeddedValue);

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
var schema = document.getElementById("schema")
if (schema) {
 codeMirrorSchema = CodeMirror.fromTextArea(schema, {
   lineNumbers: true,
   mode: "shex",
   viewportMargin: Infinity,
   matchBrackets: true
 });
}

var inputRows = getInputRows();
console.log("Creating " + inputRows + " codeMirrors");

for(i = 0; i < inputRows; i++) {
 console.log("Creating codeMirror " + i);
 newCodeMirrorNode(i);
 newCodeMirrorShape(i);
}

console.log("Document ready...");
$("#validateButton").click(function(e){
    e.preventDefault();
    console.log("click on validating...");
    var data = codeMirrorData.getValue();
    var schema = codeMirrorSchema.getValue();
    var dataFormat = $("#dataFormat").find(":selected").text();
    var schemaFormat = $("#schemaFormat").find(":selected").text();
    var schemaEngine = $("#schemaEngine").find(":selected").text();
    var triggerMode = $("#triggerMode").find(":selected").text();
    console.log("Trigger mode in AJAX query:" + triggerMode);
    var shapeMap = prepareShapeMap();
    var location = "/validate?" +
                    "data=" + encodeURIComponent(data) +
                    "&dataFormat=" + encodeURIComponent(dataFormat) +
                    "&schema=" + encodeURIComponent(schema) +
                    "&schemaFormat=" + encodeURIComponent(schemaFormat) +
                    "&schemaEngine=" + encodeURIComponent(schemaEngine) +
                    "&triggerMode=" + encodeURIComponent(triggerMode) +
                    "&shapeMap=" + shapeMap;

     $.ajax({ url: urlShaclex + "/api/validate",
      data: {
        data: data,
        schema: schema,
        dataFormat: dataFormat,
        schemaFormat: schemaFormat,
        shapeMap: shapeMap,
        schemaEngine: schemaEngine,
        triggerMode: triggerMode
     },
    type: "GET",
    dataType : "json"
  })
  .done(function(result) {
     console.log("Done!: " + JSON.stringify(result));
     showResult(result);
     history.pushState({},"validate",location);
  })
  .fail(function( xhr, status, errorThrown ) {
    $("#resultDiv").html("<h2>" + errorThrown + "</h2><pre>" + xhr.responseText + "</pre><p>" + status + "</p>" );
    console.log( "Error: " + errorThrown );
    console.log( "Status: " + status );
    console.dir( xhr );
  })
  });

  $("#permalink").click(function(e){
    e.preventDefault();
    console.log("generating permalink");
    var data = codeMirrorData.getValue();
    var schema = codeMirrorSchema.getValue();
    var dataFormat = $("#dataFormat").find(":selected").text();
    var schemaFormat = $("#schemaFormat").find(":selected").text();
    var schemaEngine = $("#schemaEngine").find(":selected").text();
    var triggerMode = $("#triggerMode").find(":selected").text();
    console.log("Trigger mode in permalink generation:" + triggerMode);
    var shapeMap = prepareShapeMap();
    var location = "/validate?" +
                    "data=" + encodeURIComponent(data) +
                    "&dataFormat=" + encodeURIComponent(dataFormat) +
                    "&schema=" + encodeURIComponent(schema) +
                    "&schemaFormat=" + encodeURIComponent(schemaFormat) +
                    "&schemaEngine=" + encodeURIComponent(schemaEngine) +
                    "&triggerMode=" + encodeURIComponent(triggerMode) +
                    "&shapeMap=" + shapeMap ;
    console.log("Permalink: " + location);
    window.location = location;
  });

 });
