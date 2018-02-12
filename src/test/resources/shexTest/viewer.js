(function () {
  if (location.search.substr(1) === "toy") { // some examples from validation/manifest.jsonld
    renderManifest(aFewTests(), "validation/");
  } else {
    $.ajaxSetup({ mimeType: "text/plain" }); // for persistent FF bug.
    $.getJSON(location.search.substr(1) + "/manifest.jsonld").then(data => {
      renderManifest(data["@graph"][0].entries, location.search.substr(1) + "/");
    }).fail(e => {
      $("table thead").append(
        $("<tr/>").append(
          $("<th/>").text("directory"),
          $("<th/>").text("description")
        ));
      $("table tbody").append(
        [
          {directory: "schemas",
           description: "ShExC, JSON and Turtle versions of all schemas in the test suite."},
          {directory: "validation",
           description: "Positive and negative ShEx validation tests."},
          {directory: "negativeStructure",
           description: "Constructs that fail <a href='http://shex.io/shex-semantics/index.html#schema-requirements'>structural constraints</a>."},
          {directory: "negativeSyntax",
           description: "Constructs that fail the <a href='http://shex.io/shex-semantics/index.html#shexc'>ShExC grammar</a>."},
          {directory: "toy",
           description: "small, static emulation of some validation tests for easy debugging."},
        ].map(d => {
          return $("<tr/>").append(
            $("<td/>").append($("<a/>", {href: location.href+"?"+d.directory}).text(d.directory)),
            $("<td/>").html(d.description)
          );
        }));
    });
  }

  let droparea = $("#droparea");
  let type = droparea.find("input");
  let data = droparea.find("textarea");
  droparea.on("dragover", function (evt) {
    droparea.addClass("droppable");
    evt.preventDefault();
  }).on("dragleave", () => {
    droparea.removeClass("droppable");
  }).on("drop", evt => {
    droparea.removeClass("droppable");
    evt.preventDefault();
    let xfer = evt.originalEvent.dataTransfer;
    const prefOrder = [
      "files", "application/json", "text/uri-list", "text/plain"
    ];
    if (prefOrder.find(l => {
      if (l.indexOf("/") === -1) {
        if (xfer[l].length > 0) {
          type.val(l);
          data.val(JSON.stringify(xfer[l]));
          readfiles(xfer[l], data);
          return true;
        }
      } else {
        if (xfer.getData(l)) {
          type.val(l);
          data.val(xfer.getData(l));
          return true;
        }
      }
      return false;
    }) === undefined) {
      type.val(xfer.types[0]);
      data.val(xfer.getData(xfer.types[0]));
    }
  }).on("dragstart", (evt) => {
    evt.originalEvent.dataTransfer.setData(type.val(), data.val());
  });
  type.on('mousedown', function(e) {
    e.stopPropagation();
    droparea.attr('draggable', false);
  }).on('mouseup', function(e) {
    droparea.attr('draggable', true);
  });

  /* progressively render the tests, adjusting relative URLs by relPrepend.
   */
  function renderManifest (tests, relPrepend) {
    let testNo = 0;
    // assumes at least one test entry
    queue();

    function queue () {
      renderTest(tests[testNo]);
      if (++testNo < tests.length)
        setTimeout(queue, 0);
      else
        $("#tests").colResizable({ fixed:false, liveDrag:true, gripInnerHtml:"<div class='grip2'></div>"});
    }

    function renderTest (test) {
      const structures = {
        "sht:ValidationTest": {
          str: "passes", chr: "✓", offset: ["action"],
          fields: [
            {name:"schema", f: link},
            {name:"data", f: link},
            {name:"shape map", f:makeShapeMap}
          ]
        },
        "sht:ValidationFailure": {
          str: "fails" , chr: "✗", offset: ["action"],
          fields: [
            {name:"schema", f: link},
            {name:"data", f: link},
            {name:"shape map", f:makeShapeMap}
          ]
        },
        "sht:RepresentationTest": {
          str: "" , chr: "", offset: [],
          fields: [
            {name:"shex", f: link},
            {name:"json", f: link},
            {name:"ttl", f:link}
          ]
        },
        "sht:NegativeStructure": {
          str: "" , chr: "", offset: [],
          fields: [
            {name:"shex", f: link}
          ]
        },
        "sht:NegativeSyntax": {
          str: "" , chr: "", offset: [],
          fields: [
            {name:"shex", f: link}
          ]
        }
      };

      let structure = structures[test["@type"]];
      if (testNo === 0) {
        // Table heading with column titles.
        $("table thead").append(
          drag("tr", { }, x => JSON.stringify(tests, null, 2), "application/json").append(
            $("<th/>"),
            $("<th/>").text("name"),
            structure.fields.map(h => {
              return $("<th/>").text(h.name);
            })
          ));
      }

      let titleText = "#" + (testNo+1) + " " + structure.str;
      let status = drag("td", { title: titleText, class: structure.str }, showTest, "application/json").text(structure.chr);
      let attrs = structure.offset.reduce((acc, o) => { return acc[o]; }, test);
      let name = drag("td", { title: test.comment }, showTest, "application/json").text(test["@id"]);
      $("table tbody").append(
        $("<tr/>").append(
          status, name,
          // $("<td/>").append(shexc), $("<td/>").append(data), shapemap
          structure.fields.map(h => {
            return h.f(attrs, h.name);
          })
        ));

      if (testNo === tests.length-1) {
        // Table footer with column titles.
        $("table tbody").append(
          drag("tr", { }, x => JSON.stringify(tests, null, 2), "application/json").append(
            $("<th/>"),
            $("<th/>").text(tests.length + " tests"),
            structure.fields.map(h => {
              return $("<th/>").text(h.name);
            })
          )
        );
      }

      function showTest (elt) {
        return JSON.stringify(test, null, 2);
      }

      function ttl (ld) {
        return typeof ld === "object" ? lit(ld) :
          ld.startsWith("_:") ? ld :
          "<" + ld + ">";
        function lit (o) {
          let ret = "\""+o["@value"]+"\"";
          if ("@type" in o)
            ret += "^^<" + o["@type"] + ">";
          if ("language" in o)
            ret += "@" + o["language"];
          return ret;
        }
      }

      function drag (name, attrs, val, type) {
        return $("<"+name+"/>", attrs).attr("draggable", true).
          on("dragstart", (evt) => {
            evt.originalEvent.dataTransfer.setData(type, val(evt.target));
            return true;
          });
      }

      function link (attrs, name) {
        let val = attrs[name];
        let a = $("<a/>", { href: relPrepend + val }).text(val);
        attrs[name] = a.prop("href");
        return title(drag("td", {}, elt => {
          return a.get(0).href;
        }, "text/uri-list").append(a), a.get(0).href);
      }

      function title (target, url) {
        $.ajax({
          url: url,
          dataType: 'text',
          type: 'GET',
          async: true
        }).then(function (data) {
          target.attr("title", data.length > 0 ? data : "-- empty file --");
        }).fail(function (jqXHR, status, errorThrown) {
          target.addClass("error");
          target.attr("title", url + " " + status + ": " + errorThrown);
        });
        return target;
      }

      function makeShapeMap (attrs, val) {
        if ("map" in attrs) {
          var a = $("<a/>", { href: relPrepend + attrs.map }).text(attrs.map);
          title(a, a.get(0).href)
          attrs["map"] = a.prop("href");
          return drag("<td/>", { }, elt => {
            return a.get(0).href;
          }, "text/uri-list").append(a);
        } else {
          return drag("td", { }, elt => {
            return elt.innerText;
          }, "text/plain").text(ttl(attrs.focus) + "@" + ("shape" in attrs ? ttl(attrs.shape) : "START"))
        }
      }
    };
  }

  function readfiles(files, target) {
    var formData = new FormData();
    for (var i = 0; i < files.length; i++) {
      var file = files[i], name = file.name;
      formData.append("file", file);
      var reader = new FileReader();
      reader.onload = (function (target) {
        return function (event) {
          target.text(event.target.result);
        };
      })(target);
      reader.readAsText(file);
    }
  }

  function aFewTests () {
    return [
      {
        "@id": "#0_otherbnode",
        "@type": "sht:ValidationTest",
        "action": {
          "schema": "../schemas/0.shex",
          "shape": "http://a.example/S1",
          "data": "Babcd_Ip1_Io1.ttl",
          "focus": "_:abcd"
        },
        "extensionResults": [],
        "name": "0_otherbnode",
        "trait": [
          "ToldBNode",
          "Empty"
        ],
        "comment": "<S1> {  } on { _:abcd <p1> <o1> }",
        "status": "mf:proposed"
      },
      {
        "@id": "#3groupdot3Extra_pass-iri1",
        "@type": "sht:ValidationTest",
        "action": {
          "schema": "../schemas/3groupdot3Extra.shex",
          "shape": "http://a.example/S1",
          "data": "Is_Ipn_IonX3.ttl",
          "focus": "http://a.example/s"
        },
        "extensionResults": [],
        "name": "3groupdot3Extra_pass-iri1",
        "trait": [
          "Extra",
          "IriEquivalence",
          "EachOf"
        ],
        "comment": "<S> EXTRA <p1> EXTRA <p2> EXTRA <p3> { <p1> [<o1>], <p2> [<o2>], <p3> [<o3>] } on { <s> <p1> <o1>; <p2> <o2>; <p3> <o3> }",
        "status": "mf:proposed"
      },
      {
        "@id": "#3circRefS1-IS2-IS3-IS3",
        "@type": "sht:ValidationTest",
        "action": {
          "schema": "../schemas/3circRefS1-IS2-IS3-IS3.shex",
          "shape": "http://a.example/S1",
          "data": "3circRefPlus1_pass-open.ttl",
          "focus": "http://a.example/n1"
        },
        "extensionResults": [],
        "name": "3circRefS1-IS2-IS3-IS3",
        "trait": [
          "Import"
        ],
        "comment": "I2 I3 <S1> { <p1> ., <p2> @<S2>? } | I3 <S2> { <p3> @<S3> } | <S3> { <p4> @<S1> } on { <n1> <p1> \"X\" ; <p2> <n2> . <n2> <p3> <n3> . <n3> <p4> <n5> . <n5> <p1> \"X\" }",
        "status": "mf:proposed",
        "result": "3circRefPlus1_pass-open.val"
      },
      {
        "@id": "#focusdatatype_pass",
        "@type": "sht:ValidationTest",
        "action": {
          "schema": "../schemas/focusdatatype.shex",
          "shape": "http://a.example/S1",
          "data": "Is1_Ip1_LabDTbloodType.ttl",
          "focus": {
            "@value": "ab",
            "@type": "http://a.example/bloodType"
          }
        },
        "extensionResults": [],
        "name": "focusdatatype_pass",
        "trait": [
          "FocusConstraint"
        ],
        "comment": "<S> <dt1> on { <s1> <p1> 'ab'^^my:bloodType }",
        "status": "mf:proposed"
      },
      {
        "@id": "#dependent_shape",
        "@type": "sht:ValidationTest",
        "action": {
          "schema": "../schemas/dependent_shape.shex",
          "data": "dependent_shape.ttl",
          "map": "dependent_shape_map.json"
        },
        "extensionResults": [],
        "name": "dependent_shape",
        "trait": [
          "ShapeMap"
        ],
        "comment": "<S1> {<p1> @<S2>} <S2> {<p2> [<s3>]} on { <s1> <p1> <s2> . <s2> <p2> <s3> .}",
        "status": "mf:proposed",
        "result": "dependent_shape_results.json"
      },
    ];
  }
})();

