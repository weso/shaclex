function escapeHtml(unsafe) {
    return unsafe
        .replace(/&/g, "&amp;")
        .replace(/</g, "&lt;")
        .replace(/>/g, "&gt;")
        .replace(/"/g, "&quot;")
        .replace(/'/g, "&#039;");
}

function flatten(examples) {
    console.log("Examples: ");
    console.log(examples);
    var result = [] ;
    var DefaultSchemaFormat = "ShExC" ;
    var DefaultSchemaEngine = "ShEx" ;
    var DefaultDataFormat = "Turtle" ;
    var DefaultTriggerMode = "ShapeMap" ;

    if (typeof examples == "string") {
      console.log("Examples = string ");
      var r = JSON.parse(examples);
      console.log(r);
      return result;
    } else {
    examples.forEach(function (entry,idx) {
      var schemaFormat = getOrElse(entry.schemaFormat, DefaultSchemaFormat);
      var schemaEngine = getOrElse(entry.schemaEngine, DefaultSchemaEngine);
      var dataFormat = getOrElse(entry.dataFormat, DefaultDataFormat);
      var triggerMode = getOrElse(entry.triggerMode, DefaultTriggerMode);

      result.push({
             "name": entry.schemaLabel,
             "schemaURL": entry.schemaURL,
             "dataURL": entry.dataURL,
             "shapeMap": entry.queryMap,
             "passes": (entry.status == "conformant" ? true: false),
             "descr": entry.dataLabel,
             "dataFormat": dataFormat,
             "schemaFormat": schemaFormat,
             "schemaEngine": schemaEngine,
             "triggerMode": triggerMode
           })
        });
    return result;
    }
}


function getHost() {
    var port = window.location.port;
    return window.location.protocol + "//" +
        window.location.hostname + (port? ":" + port: "") ;
}

function getOrElse(value, defaultValue) {
 if (value) return value
 else return defaultValue
}

$(document).ready(function() {
 var examplesUrl = $("#examples").data("examples");
 console.log(examplesUrl);
 var urlService = getHost() + "/validate";
 $.ajax({ url: examplesUrl, dataType: 'json'}).done(function(examples) {
   var flattened = flatten(examples);
   flattened.forEach(function(entry,idx) {
     var name = $("<td>").text(entry.name) ;
     var schemaURL = $("<td>").append($("<a>").attr("href", entry.schemaURL).text(entry.schemaURL));
     var schemaFormat = $("<td>").text(entry.schemaFormat);
     var dataURL = $("<td>").append($("<a>").attr("href", entry.dataURL).text(entry.dataURL)) ;
     var dataFormat = $("<td>").text(entry.dataFormat);
     var shapeMap = $("<td>").text(entry.shapeMap) ;
     var descr = $("<td>").text(entry.descr) ;
     var triggerMode = $("<td>").text(entry.triggerMode) ;
     var schemaEngine = $("<td>").text(entry.schemaEngine) ;

     $.ajax({url: entry.dataURL,
             dataType: 'text'}).done(function(dataContents) {
      //            dataURL.html(data);
      $.ajax({url: entry.schemaURL,
              dataType: 'text'}
             ).done(function (schemaContents) {
      //               schemaURL.html(schema);
      /*  var serviceData =  { data: dataContents,
                             schema: schemaContents,
                             shapeMap: entry.shapeMap,
                             schemaFormat: entry.schemaFormat,
                             dataFormat: entry.dataFormat
                           };
        console.log("Service data: " + serviceData); */
        var tryIt = $("<form>").
          attr("method","POST").
          attr("action",urlService).
          append($("<input>").attr("type","hidden").attr("name","data").attr("value",dataContents)).
          append($("<input>").attr("type","hidden").attr("name","schema").attr("value",schemaContents)).
          append($("<input>").attr("type","hidden").attr("name","schemaFormat").attr("value",entry.schemaFormat)).
          append($("<input>").attr("type","hidden").attr("name","dataFormat").attr("value",entry.dataFormat)).
          append($("<input>").attr("type","hidden").attr("name","triggerMode").attr("value",entry.triggerMode)).
          append($("<input>").attr("type","hidden").attr("name","shapeMap").attr("value",entry.shapeMap)).
          append($("<input>").attr("type","hidden").attr("name","schemaEngine").attr("value",entry.schemaEngine)).
          append($("<button>").
                 addClass("btn").
                 addClass("btn-primary").
                 attr("type","submit").
                 text("Try it")
                 );
/*        var tryIt = $("<td>").append($("<button>").addClass("btn").addClass("btn-primary").text("Try it").click(function() {
                            console.log("Clicked")
                            var tryItRequest = $.ajax({
                                method: "POST",
                                url: urlService,
                                data: serviceData,
                                success: function(result) {
                                  console.log("OK result: " + result);
                                  $("html").html(result);
                                },
                                error: function() {
                                  console.log("An error has been produced");
                                 }
                                });
                            tryItRequest.done();
                            tryItRequest.fail(function() {
                                console.log();
                            });
                        }));
*/
//        var tryIt = $("<td>").append($("<a>").attr("href", tryItURL).text("Try It"));
        var tr = $("<tr>").append(name,schemaURL,dataURL,shapeMap,descr,triggerMode, schemaEngine, tryIt);
        $("#examples").append(tr);
      });
     });
   });
 });
});
