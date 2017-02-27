function changeMode(element,syntax) {
 var mode = "turtle";
 switch (syntax.toUpperCase()) {
  case 'TURTLE': mode = 'turtle' ;
                 break ;
  case 'N-TRIPLES': mode = 'turtle' ;
                 break ;
  case 'RDF/XML': mode = 'xml' ;
                 break ;
  case 'JSON-LD': mode = 'javascript' ;
                 break ;
  case 'SHEXC': mode = 'shex' ;
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
 if (value=='schemaEmbedded') {
  $("#schemaDiv").hide();
 } else {
  $("#schemaDiv").show();
 }
}

function changeTriggerMode(value) {
 console.log("Changing triggermode: " + value);
 switch (value.toUpperCase()) {
  case 'TARGETDECLS':
    $("#nodeDiv").hide();
    $("#shapeDiv").hide();
    console.log("Hiding all: " + value);
    break;
  case 'NODESHAPE':
    $("#nodeDiv").show();
    $("#shapeDiv").show();
    console.log("Showing all: " + value);
    break;
  case 'NODESTART':
    $("#nodeDiv").show();
    $("#shapeDiv").hide();
    console.log("Showing node only: " + value);
    break;
 }
}

function getDataFormat(element) {
 var format = element.options[element.selectedIndex].value;
 window.alert("Data format of " + element + " format: " + format);
}

$(document).ready(function(){

// var urlShaclex = "http://shaclex.herokuapp.com"
var urlShaclex = "http://localhost:8080";
console.log("Main Url = " + urlShaclex);

var schemaEmbeddedValue = $("#toggleSchemaEmbedded").val();
console.log("Schema embedded = " + schemaEmbeddedValue);
changeSchemaEmbedded(schemaEmbeddedValue);

var triggerModeValue = $("#triggerMode").val();
console.log("Trigger mode = " + triggerModeValue);
changeTriggerMode(triggerModeValue);

var rdfData = document.getElementById("rdfData");

var codeMirrorData = CodeMirror.fromTextArea(document.getElementById("rdfData"), {
 lineNumbers: true,
 mode: "turtle",
});

var codeMirrorSchema = CodeMirror.fromTextArea(document.getElementById("schema"), {
  lineNumbers: true,
  mode: "shex",
});

// Don't allow newline before change in CodeMirror
function noNewLine(instance,change) {
    var newtext = change.text.join("").replace(/\n/g, ""); // remove ALL \n !
    change.update(change.from, change.to, [newtext]);
    return true;
}

var codeMirrorNode = CodeMirror.fromTextArea(document.getElementById("node"), {
 lineNumbers: false,
 mode: "turtle",
 scrollbarStyle: "null",
 height: 1,
});

var codeMirrorShape = CodeMirror.fromTextArea(document.getElementById("shape"), {
 lineNumbers: false,
 mode: "turtle",
 scrollbarStyle: "null",
 height: 1,
});

codeMirrorNode.on("beforeChange", noNewLine);
codeMirrorShape.on("beforeChange", noNewLine);


 console.log("Document ready...");
  $("#validateButton").click(function(e){
     e.preventDefault();
     console.log("click on validating...");
     var data = codeMirrorData.getValue();
     var schema = codeMirrorSchema.getValue();
     var dataFormat = $("#dataFormat").val();
     var schemaFormat = $("#schemaFormat").val();
     var node = codeMirrorNode.getValue();
     var shape = codeMirrorShape.getValue();
     var schemaEngine = $("#schemaEngine").val();
     var triggerMode = $("#triggerMode").val();

     $.ajax({ url: urlShaclex + "/api/validate",
      data: {
        data: data,
        schema: schema,
        dataFormat: dataFormat,
        schemaFormat: schemaFormat,
        node: node,
        shape: shape,
        schemaEngine: schemaEngine,
        triggerMode: triggerMode
     },
    type: "GET",
    dataType : "json"
  })
  .done(function(json) {
     var result = json.result;
     console.log("Done!" + JSON.stringify(json));

     var validClass = result.valid ? "valid": "notValid" ;
     $("#resultDiv").addClass(validClass);
     $("#resultDiv").empty();
     $("#resultDiv").append($("<h2>").text(validClass));
     var pre = $("<pre/>").html(JSON.stringify(result,undefined,2));
     var details = $("<details/>").append(pre);
     $("#resultDiv").append(details);
     window.history.pushState("validate", "Validate", "/validate?" +
       "data=" + encodeURI(data) +
       "&dataFormat=" + encodeURI(dataFormat) +
       "&schema=" + encodeURI(schema) +
       "&schemaFormat=" + encodeURI(schemaFormat) +
       "&schemaEngine=" + encodeURI(schemaEngine) +
       "&triggerMode=" + encodeURI(triggerMode) +
       "&node=" + encodeURI(node) +
       "&shape=" + encodeURI(shape)
       );
  })
  .fail(function( xhr, status, errorThrown ) {
    $("#resultDiv").html("<h2 class='notValid'>" + errorThrown + "<pre>" + xhr.responseText + "</pre><p>" + status + "</p></h2>" );
    console.log( "Error: " + errorThrown );
    console.log( "Status: " + status );
    console.dir( xhr );
  })
  });

  $("#permalink").click(function(e){
    e.preventDefault();
    var data = codeMirrorData.getValue();
    var schema = codeMirrorSchema.getValue();
    var dataFormat = $("#dataFormat").val();
    var schemaFormat = $("#schemaFormat").val();
    var node = codeMirrorNode.getValue();
    var shape = codeMirrorShape.getValue();
    var schemaEngine = $("#schemaEngine").val();
    var triggerMode = $("#triggerMode").val();
    var location = "/validate?" +
                    "data=" + encodeURI(data) +
                    "&dataFormat=" + encodeURI(dataFormat) +
                    "&schema=" + encodeURI(schema) +
                    "&schemaFormat=" + encodeURI(schemaFormat) +
                    "&schemaEngine=" + encodeURI(schemaEngine) +
                    "&triggerMode=" + encodeURI(triggerMode) +
                    "&node=" + encodeURI(node) +
                    "&shape=" + encodeURI(shape);
    console.log("Permalink: " + location);
    window.location = location;
  });

 });
