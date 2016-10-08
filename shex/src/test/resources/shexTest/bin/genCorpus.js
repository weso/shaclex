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
var FROMDIR = null;
if (args[1] === "-d") {
  FROMDIR = args[2];
  args.splice(1, 2); // git rid of -d fromdir
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

var files = fs.readdirSync(FROMDIR).filter(fn => {
  // console.warn(path.extname(fn) === ".json", path.relative(".", path.resolve(FROMDIR, fn)), path.relative(".", OUTFILE));
  // old strategy: look for all json files !== to the out corpus
  // return path.extname(fn) === ".json" &&
  //   path.relative(".", path.resolve(FROMDIR, fn)) !== path.relative(".", OUTFILE);
  // new strategy: look for a .shex files
  return path.extname(fn) === ".shex"
}).map(fn => {
  var base = path.basename(fn, path.extname(fn));
  // var rel = path.relative(".", path.join(path.dirname(fn), base));
  return base;
});
// console.warn(files);
var corpus = files.map(stem => {
  // console.warn(fn);
  try {
    var js = JSON.parse(fs.readFileSync(path.resolve(FROMDIR, stem + ".json"), "utf8"));
    if (false && "shapes" in js) // change shapes from hash(k->{o}) to array[{name:k, o}]
      js.shapes = Object.keys(js.shapes).map(k => {
	var ret = js.shapes[k];
	ret.name = k;
	return ret;
      });
    return {
      filename: stem,
      shex: fs.readFileSync(path.resolve(FROMDIR, stem + ".shex"), "utf8"),
      json: js
    };
  } catch (e) {
    var e2 = Error(`in file ${path.relative(".", path.resolve(FROMDIR, fn))}, ${e}`);
    e2.stack = `in file ${path.relative(".", path.resolve(FROMDIR, fn))}, ${e.stack}`;
    throw e2;
  }
});
fs.writeFileSync(OUTFILE, JSON.stringify(corpus, null, "  "), {encoding: "utf8"});
