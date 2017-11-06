/*var json = [ {
 "name": "bibframe book",
 "schemaURL": "examples/shex/book.shex",
  "passes": [ {
    "name": "simple",
    "dataURL": "examples/shex/book.ttl",
    "queryMap": "<samples9298996>@<Work>"
  }],
  "fails": [ {
     "name": "no bf:label",
      "dataURL": "examples/shex/book-no-bf-label.ttl",
      "queryMap": "<samples9298996>@<Work>"
   },
            {
                "name": "value set violation",
                "dataURL": "examples/shex/book-value-set-violation.ttl",
                "queryMap": "<samples9298996>@<Work>"
            }
        ]
    },
    {
        "name": "reduced bibframe book",
        "schemaURL": "examples/shex/book-small.shex",
        "passes": [
            {
                "name": "simple",
                "dataURL": "examples/shex/book-small.ttl",
                "queryMap": "<samples9298996>@<Work>"
            }
        ],
        "fails": [
            {
                "name": "no bf:label",
                "dataURL": "examples/shex/book-small-no-bf-label.ttl",
                "queryMap": "<samples9298996>@<Work>"
            },
            {
                "name": "value set violation",
                "dataURL": "examples/shex/book-small-value-set-violation.ttl",
                "queryMap": "<samples9298996>@<Work>"
            }
        ]
    },
    {
        "name": "bibframe book opt bf:label",
        "schemaURL": "examples/shex/book-opt-bf-label.shex",
        "passes": [
            {
                "name": "no bf:label",
                "dataURL": "examples/shex/book-no-bf-label.ttl",
                "queryMap": "<samples9298996>@<Work>"
            }
        ],
        "fails": [
        ]
    }
] ;

*/

function escapeHtml(unsafe) {
    return unsafe
        .replace(/&/g, "&amp;")
        .replace(/</g, "&lt;")
        .replace(/>/g, "&gt;")
        .replace(/"/g, "&quot;")
        .replace(/'/g, "&#039;");
}

function flatten(examples) {
    var result = [] ;
    var DefaultSchemaFormat = "ShExC" ;
    var DefaultSchemaEngine = "ShEx" ;
    var DefaultDataFormat = "Turtle" ;

    examples.forEach(function (entry,idx) {
      var schemaFormat = getOrElse(entry.schemaFormat, DefaultSchemaFormat);
      var schemaEngine = getOrElse(entry.schemaEngine, DefaultSchemaEngine);
      if (entry.passes) {
         entry.passes.forEach(function(passes,idx) {
         var dataFormat = getOrElse(entry.dataFormat, DefaultDataFormat);
         result.push({
             "name": entry.name,
             "schemaURL": entry.schemaURL,
             "dataURL": passes.dataURL,
             "shapeMap": passes.queryMap,
             "passes": true,
             "descr": passes.name,
             "dataFormat": dataFormat,
             "schemaFormat": schemaFormat,
             "schemaEngine": schemaEngine
           })
        });
      }
      if (entry.fails) {
       entry.fails.forEach(function(fails,idx) {
         var dataFormat = getOrElse(entry.dataFormat, DefaultDataFormat);
         result.push({
             "name": entry.name,
             "schemaURL": entry.schemaURL,
             "dataURL": fails.dataURL,
             "shapeMap": fails.queryMap,
             "passes": false,
             "descr": fails.name,
             "dataFormat": dataFormat,
             "schemaFormat": schemaFormat,
             "schemaEngine": schemaEngine
             })
        });
      }
    });
    return result;
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
 $.ajax(examplesUrl).done(function(examples) {
   var flattened = flatten(examples);
   flattened.forEach(function(entry,idx) {
     var name = $("<td>").text(entry.name) ;
     var schemaURL = $("<td>").append($("<a>").attr("href", entry.schemaURL).text(entry.schemaURL));
     var dataURL = $("<td>").append($("<a>").attr("href", entry.dataURL).text(entry.dataURL)) ;
     var shapeMap = $("<td>").text(entry.shapeMap) ;
     var descr = $("<td>").text(entry.descr) ;

     $.ajax(entry.dataURL).done(function(data) {
//            dataURL.html(data);
      $.ajax(entry.schemaURL).done(function (schema) {
//               schemaURL.html(schema);
        var host = getHost();
        var tryItURL = host + "/validate?" + "data=" + encodeURIComponent(data) + "&"
            + "schema=" + encodeURIComponent(schema) + "&"
            + "shapeMap=" + encodeURIComponent(entry.shapeMap) + "&"
            + "schemaFormat=" + encodeURIComponent(entry.schemaFormat) + "&"
            + "dataFormat=" + encodeURIComponent(entry.dataFormat) + "&"
            + "schemaEngine=" + encodeURIComponent(entry.schemaEngine) + "&"
            // TODO: Add targetDecls triggerMode
            + "triggerMode=" + encodeURIComponent("ShapeMap") + "&"
            ;

        var tryIt = $("<td>").append($("<a>").attr("href", tryItURL).text("Try It"));
        var tr = $("<tr>").append(name,schemaURL,dataURL,shapeMap,descr,tryIt);
        $("#examples").append(tr);
      });
     });
   });
 });
});
