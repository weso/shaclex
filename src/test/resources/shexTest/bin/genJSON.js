#!/usr/bin/env node

/* genJSON - tool to generate JSON-LD version of manifest.ttl
 * install and run:

git clone git@github.com:shexSpec/shexTest.git
npm install
cd validation/
../bin/genJSON.js manifest.ttl -w

*/

// Parse arguments
var args = process.argv.slice(2);
if (args > 1 || args.indexOf("-help") !== -1 || args.indexOf("--help") !== -1) {
  console.error('usage: genJSON manifest.ttl [-o outfile] [-w|-e] > manifest.jsonld');
  return process.exit(1);
}
var OUTFILE = null;
if (args[1] === "-o") {
  OUTFILE = args[2];
  args.splice(1, 2); // git rid of -o outfile
}
var errors = 0;
var WARN = args[1] === "-w" ? "warn" : args[1] === "-e" ? "err" : null;
function report (msg) {
  console.warn(msg);
  if (WARN === "err") {
    ++errors;
  }
}

var fs = require('fs');
var path = require("path");
var N3 = require("n3");
var parser = N3.Parser({blankNodePrefix: ""});
var util = N3.Util;
var store = N3.Store();
//var json = fs.readFileSync(args[0]).toString();

var P = {
  "rdf":  "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
  "rdfs": "http://www.w3.org/2000/01/rdf-schema#",
  "mf":   "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#",
  "sht":  "http://www.w3.org/ns/shacl/test-suite#",
  "sx":   "https://shexspec.github.io/shexTest/ns#"
};

var testDir = path.basename(path.dirname(path.resolve(args[0])));
var basePath = "https://raw.githubusercontent.com/shexSpec/shexTest/master/";
var dirPath = basePath + testDir + '/';
var apparentBase = dirPath + "manifest";

parser.parse(
  "@base <" + apparentBase + "> .\n"+
  fs.readFileSync(args[0], "utf8"),
  function (error, triple, prefixes) {
    if (error) {
      error.message = "Error parsing " + args[0] + ": " + error.message;
      throw error;
    }
    if (triple)
      store.addTriple(triple)
    else
      genText();
  });

/** expandCollection - N3.js utility to return an rdf collection's elements.
*/
function expandCollection (h) {
  if (store.find(h.object, "rdf:first", null).length) {
    var ret = [];
    while (h.object !== "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil") {
      ret.push(store.find(h.object, "rdf:first", null)[0].object);
      h = store.find(h.object, "rdf:rest", null)[0];
    }
    return ret;
  } else {
    return h.object
  }
}

function genText () {
  var g = []; // stuff everything into a JSON-LD @graph
  var ret = {
    "@context": [
      {"@base": apparentBase},
      "../context.jsonld"
    ],
    "@graph": g
  };

  store.addPrefixes(P);

  var manifest = store.find(null, "rdf:type", "mf:Manifest")[0].subject;
  var manifestComment = util.getLiteralValue(store.find(manifest, "rdfs:comment", null)[0].object);
  var entries = [];
  var knownMissing = {}; // record missing files.
  var head = store.find(manifest, "mf:entries", null)[0].object;
  while (head !== P.rdf + "nil") {
    entries.push(store.find(head, "rdf:first", null)[0].object);
    head = store.find(head, "rdf:rest", null)[0].object;
  }
  var unmatched = entries.reduce(function (ret, ent) {
    ret[ent] = true;
    return ret;
  }, {});
  var expectedTypes = ["ValidationTest", "ValidationFailure", "RepresentationTest", "NegativeSyntax", "NegativeStructure"].map(function (suffix) {
    return P.sht + suffix;
  });

  g.push({
    "@id": "",
    "@type": "mf:Manifest",
    "rdfs:comment": manifestComment,
    "entries": store.find(null, "rdf:type", null).filter(function (t) {
      var ret = expectedTypes.indexOf(t.object) !== -1;
      if (ret === false &&
          t.object !== P.mf + "Manifest") {
        report("test " + t.subject + " has unexpected type " + t.object);
      }
      return ret;
    }).map(function (t) {
      return [t.subject, t.object];
    }).filter(function (t) {
      var ret = entries.indexOf(t[0]) !== -1;
      if (ret === false) {
        report("unreferenced test: " + t[0]);
      } else {
        delete unmatched[t[0]];
      }
      return ret;
    }).sort(function (l, r) {
      return l[0] === r[0] ? 0 :
        entries.indexOf(l[0]) > entries.indexOf(r[0]) ? 1 :
        -1;
    }).map(function (st) {
      var s = st[0], t = st[1];
      var testName = util.getLiteralValue(store.find(s, "mf:name", null)[0].object);
      var testType = store.find(s, "rdf:type", null)[0].object.replace(P.sht, '');
      var expectedName = s.substr(apparentBase.length+1);
      if (WARN && testName !== expectedName) {
	report("expected label \"" + expectedName + "\" ; got \"" + testName + "\"");
      }
      var actionTriples = store.find(s, "mf:action", null);
      function exists (filename) {
        var filepath = path.join(__dirname, "../" + testDir + '/' + filename);
        if (WARN && !fs.existsSync(filepath) && !(filepath in knownMissing)) {
          report("non-existent file: " + filepath.substr(dirPath.length) + " is missing " + path.relative(process.cwd(), filepath));
	  knownMissing[filepath] = path.relative(process.cwd(), filepath);
        }
        return filename;
      }
      if (actionTriples.length !== 1) {
        if (["RepresentationTest", "NegativeSyntax", "NegativeStructure"].indexOf(testType) === -1) {
          throw Error("expected 1 action for " + s + " -- got " + actionTriples.length);
        }
        // Representation/Syntax/Structure tests
        return [
          //      ["rdf"  , "type"    , function (v) { return v.substr(P.sht.length); }],
          [s, "mf"   , "name"    , function (v) { return util.getLiteralValue(v[0]); }],
          [s, "sht", "trait"  , function (v) {
           return v.map(function (x) {
             return x.substr(P.sht.length);;
           });
          }],
          //[s, "rdfs" , "comment" , function (v) { return util.getLiteralValue(v[0]); }],
          [s, "mf", "status"  , function (v) { return "mf:"+v[0].substr(P.mf.length); }],
          [s, "sx", "shex", function (v) { return exists(v[0].substr(dirPath.length)); }],
          [s, "sx", "json", function (v) { return exists(v[0].substr(dirPath.length)); }],
          [s, "sx", "ttl", function (v) { return exists(v[0].substr(dirPath.length)); }],
        ].reduce(function (ret, row) {
          var found = store.findByIRI(row[0], P[row[1]]+row[2], null).map(expandCollection);
          var target = ret;
          if (found.length)
            target[row[2]] = row[3](found);
          return ret;
        }, {"@id": s.substr(apparentBase.length), "@type": "sht:"+t.substr(P.sht.length)});
      }
      var a = actionTriples[0].object;
      return [
        //      ["rdf"  , "type"    , function (v) { return v.substr(P.sht.length); }],
        [s, "mf"   , "name"    , function (v) { return util.getLiteralValue(v[0]); }],
        [s, "sht", "trait"  , function (v) {
          return v.map(function (x) {
            return x.substr(P.sht.length);;
          });
        }],
        [s, "rdfs" , "comment" , function (v) { return util.getLiteralValue(v[0]); }],
        [s, "mf", "status"  , function (v) { return "mf:"+v[0].substr(P.mf.length); }],
        [a, "sht", "schema"  , function (v) { return exists("../" + v[0].substr(basePath.length)); } ], // could be more judicious in creating a relative path from an absolute path.
        [a, "sht", "shape"   , function (v) { return v[0].indexOf(dirPath) === 0 ? v[0].substr(dirPath.length) : v[0]; }],
        [a, "sht", "data"    , function (v) { return exists(v[0].substr(dirPath.length)); }],
        [a, "sht", "map"    , function (v) { return exists(v[0].substr(dirPath.length)); }],
        [a, "sht", "focus"   , function (v) {
          // Focus can be a literal
          if (util.isLiteral(v[0])) {
            var lang = util.getLiteralLanguage(v[0]);
            var dt = util.getLiteralType(v[0]);
            var res = {'@value': util.getLiteralValue(v[0])};
            if (lang.length > 0) {res['@language'] = lang}
            if (dt.length > 0) {res['@type'] = dt}
            return res;
          } else {
            return (v[0].indexOf(dirPath) === 0 ? v[0].substr(dirPath.length) : v[0]);
          }
        }],
        [a, "sht", "semActs" , function (v) { return exists("../" + v[0].substr(basePath.length)); }], // could be more judicious in creating a relative path from an absolute path.
        [a, "sht", "shapeExterns" , function (v) { return exists("../" + v[0].substr(basePath.length)); }], // could be more judicious in creating a relative path from an absolute path.
        [s, "mf", "result"  , function (v) { return exists(v[0].substr(dirPath.length)); }],
        [s, "mf", "extensionResults"  , function (v) {
          return v[0].map(function (x) {
            return {
              extension: store.find(x, "mf:extension", null)[0].object,
              prints: util.getLiteralValue(store.find(x, "mf:prints", null)[0].object)
            };
          });
        }]
      ].reduce(function (ret, row) {
        var found = store.findByIRI(row[0], P[row[1]]+row[2], null).map(expandCollection);
        var target = row[0] === s ? ret : row[0] === a ? ret.action : ret.extensionResults;
        if (found.length)
          target[row[2]] = row[3](found);
        return ret;
      }, {"@id": s.substr(apparentBase.length), "@type": "sht:"+t.substr(P.sht.length), action: {}, extensionResults: []});
    })
  });
  var remaining = Object.keys(unmatched);
  if (remaining.length) {
    report("no definitions for " + remaining.join(", "));
  }
  if (!errors) {
    if (OUTFILE) {
      fs.writeFileSync(OUTFILE, JSON.stringify(ret, null, "  "));
    } else {
      console.log(JSON.stringify(ret, null, "  "));
    }
    process.exit(0);
  } else {
    process.exit(1);
  }
}
