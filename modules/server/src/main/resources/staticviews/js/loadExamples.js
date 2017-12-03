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
    var DefaultInference = "None" ;

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
            var inference = getOrElse(entry.inference, DefaultInference);
            result.push({
                "name": entry.schemaLabel,
                "schemaURL": entry.schemaURL,
                "schema": entry.schema,
                "dataURL": entry.dataURL,
                "data": entry.data,
                "shapeMap": entry.queryMap,
                "status": entry.status ,
                "descr": entry.dataLabel,
                "dataFormat": dataFormat,
                "schemaFormat": schemaFormat,
                "schemaEngine": schemaEngine,
                "inference": inference,
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

function mkRelative(url, base) {
 if (url)
   return new URL(url, base);
 else
   return url;
}

function cbSchema(entry, dataContents) {
 if (entry.schemaURL) {
 //            dataURL.html(data);
 $.ajax({url: entry.schemaURL,
     dataType: 'text'}
  ).done(function(schemaContents) { mkTryItButtom(entry, dataContents, schemaContents) })
   .fail(function (jqXHR, textStatus) {
     var tr = $("<tr>").append($("<td>").attr("colspan", 8).text("Failed to load schemaURL: " + entry.schemaURL + " Error:" + jqXHR.status + "-" + jqXHR.responseText));
     $("#examples").append(tr);
  });
 } else {
  mkTryItButtom(entry, dataContents, getOrElse(entry.schema, ""))
 }
}

function mkTryItButtom(entry, dataContents, schemaContents) {
   var schemaURL = $("<td>");
   if (entry.schemaURL) {
    schemaURL.append($("<a>").attr("href", entry.schemaURL).text(entry.name));
   } else {
   schemaURL.text(entry.name);
  }
  var schemaFormat = $("<td>").text(entry.schemaFormat);
  var dataURL = $("<td>");
  if (entry.dataURL) {
   dataURL.append($("<a>").attr("href", entry.dataURL).text(entry.descr)) ;
  } else {
   dataURL.text(entry.descr);
  }
  var dataFormat = $("<td>").text(entry.dataFormat);
  var inference = $("<td>").text(entry.inference);
  var details = $("<details>");
  details.append($("<summary>").text(entry.triggerMode),
                           $("<code>").text(entry.shapeMap));
  var shapeMap = $("<td>").append(details);
  var schemaEngine = $("<td>").text(entry.schemaEngine) ;
  console.log(entry.status);
  var buttonClass = entry.status == "conformant" ? "btn-success" : "btn-danger" ;
  var tryIt = $("<td>").append($("<form>").
     attr("method","POST").
     attr("enctype","multipart/form-data").
     attr("action",urlService()).
     append($("<input>").attr("type","hidden").attr("name","data").attr("value",dataContents)).
     append($("<input>").attr("type","hidden").attr("name","schema").attr("value",schemaContents)).
     append($("<input>").attr("type","hidden").attr("name","schemaFormat").attr("value",entry.schemaFormat)).
     append($("<input>").attr("type","hidden").attr("name","dataFormat").attr("value",entry.dataFormat)).
     append($("<input>").attr("type","hidden").attr("name","triggerMode").attr("value",entry.triggerMode)).
     append($("<input>").attr("type","hidden").attr("name","shapeMap").attr("value",entry.shapeMap)).
     append($("<input>").attr("type","hidden").attr("name","inference").attr("value",entry.inference)).
     append($("<input>").attr("type","hidden").attr("name","schemaEngine").attr("value",entry.schemaEngine)).
     append($("<button>").
      addClass("btn").
      addClass(buttonClass).
      attr("type","submit").
      text("Try it")
     ));
     var tr = $("<tr>").append(schemaURL,schemaFormat,dataURL,dataFormat, shapeMap, schemaEngine, inference, tryIt);
     $("#examples").append(tr);
}

function urlService() {
    return getHost() + "/validate";
}

$(document).ready(function() {
    var examplesUrl = $("#examples").data("examples");
    console.log(examplesUrl);
    $.ajax({ url: examplesUrl, dataType: 'json'}).done(function(examples) {
         var flattened = flatten(examples);
         flattened.forEach(function(entry,idx) {
           entry.schemaURL = mkRelative(entry.schemaURL, examplesUrl);
           entry.dataURL = mkRelative(entry.dataURL, examplesUrl);
           if (entry.dataURL) {
           $.ajax({url: entry.dataURL,
                    dataType: 'text'})
             .done(function(dataContents) { cbSchema(entry,dataContents) })
             .fail(function (jqXHR, textStatus) {
                var tr = $("<tr>").append($("<td>").attr("colspan", 8).text("Failed to load dataURL: " + entry.dataURL + " Error:" + jqXHR.status + "-" + jqXHR.responseText));
                $("#examples").append(tr);
            });
           } else {
             cbSchema(entry, getOrElse(entry.data, ""));
           }
        });
    });
});
