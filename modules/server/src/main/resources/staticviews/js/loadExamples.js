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
                "dataURL": entry.dataURL,
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

$(document).ready(function() {
    var examplesUrl = $("#examples").data("examples");
    console.log(examplesUrl);
    var urlService = getHost() + "/validate";
    $.ajax({ url: examplesUrl, dataType: 'json'}).done(function(examples) {
        var flattened = flatten(examples);
        flattened.forEach(function(entry,idx) {
//            var name = $("<td>").text(entry.name) ;
            var schemaURL = $("<td>").append($("<a>").attr("href", entry.schemaURL).text(entry.name));
            var schemaFormat = $("<td>").text(entry.schemaFormat);
            var dataURL = $("<td>").append($("<a>").attr("href", entry.dataURL).text(entry.descr)) ;
            var dataFormat = $("<td>").text(entry.dataFormat);
            var inference = $("<td>").text(entry.inference);
            var details = $("<details>");
            details.append($("<summary>").text(entry.triggerMode),
                           $("<code>").text(entry.shapeMap));
            var shapeMap = $("<td>").append(details);
            var schemaEngine = $("<td>").text(entry.schemaEngine) ;
            $.ajax({url: entry.dataURL,
                    dataType: 'text'}).done(function(dataContents) {
                //            dataURL.html(data);
                $.ajax({url: entry.schemaURL,
                    dataType: 'text'}
                ).done(function (schemaContents) {
                    console.log(entry.status);
                    var buttonClass = entry.status == "conformant" ? "btn-success" : "btn-danger" ;
                    var tryIt = $("<td>").append($("<form>").
                    attr("method","POST").
                    attr("action",urlService).
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
                    var tr = $("<tr>").append(schemaURL,schemaFormat,dataURL,dataFormat, shapeMap,schemaEngine, inference, tryIt);
                    $("#examples").append(tr);
                });
            });
        });
    });
});
