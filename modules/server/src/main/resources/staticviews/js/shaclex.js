// var urlShaclex = "http://shaclex.herokuapp.com"
var urlShaclex = "http://localhost:8080";

var rdfData = document.getElementById("rdfData");
var codeMirrorData = CodeMirror.fromTextArea(document.getElementById("rdfData"), {
 lineNumbers: true,
 mode: "turtle"
});
var codeMirrorSchema = CodeMirror.fromTextArea(document.getElementById("schema"), {
        lineNumbers: true,
        mode: "shex"
});

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

function getDataFormat(element) {
 var format = element.options[element.selectedIndex].value;
 window.alert("Data format of " + element + " format: " + format);
}

$(document).ready(function(){
 console.log("Document ready...");
  $("#validateButton").click(function(e){
     e.preventDefault();
     console.log("click on validating...");
     $.ajax({ url: urlShaclex + "/api/validate",
      data: {
        data: codeMirrorData.getValue(),
        schema: codeMirrorSchema.getValue(),
        dataFormat: $("#dataFormat").val(),
        schemaFormat: $("#schemaFormat").val(),
        node: $("#node").val(),
        shape: $("#shape").val(),
        schemaEngine: $("#schemaEngine").val(),
        triggerMode: "NodeShape"
     },
    type: "GET",
    dataType : "json"
  })
  .done(function(json) {
     var result = json.result;
     console.log("Done!" + JSON.stringify(json));
     var validClass = result.valid ? "valid": "notValid";
     var pre = $("<pre/>", { "class": validClass })
     pre.html(JSON.stringify(result,undefined,2));
     $("#resultDiv").empty();
     $("#resultDiv").append(pre);
  })
  .fail(function( xhr, status, errorThrown ) {
    $("#resultDiv").html("<h2 class='notValid'>" + errorThrown + "<pre>" + xhr.responseText + "</pre><p>" + status + "</p></h2>" );
    console.log( "Error: " + errorThrown );
    console.log( "Status: " + status );
    console.dir( xhr );
  })
  // Code to run regardless of success or failure;
  .always(function( xhr, status ) {
    console.log( "Always...: " + status );
  })

  })

 });
