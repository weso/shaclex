#!/usr/bin/env node

/* genCorpus - tool to generate a schema corpus
 * install and run:

git clone git@github.com:shexSpec/shexTest.git
cd shexTest/
npm install
./bin/genCorpus.js -d schemas/ -o corpus.json

*/

var fs = require('fs');
var path = require("path");

// Parse arguments
var args = process.argv.slice(2);
if (args > 1 || args.indexOf("-help") !== -1 || args.indexOf("--help") !== -1) {
  console.error('usage: genJSON manifest.ttl [-o outfile] [-w|-e] > manifest.jsonld');
  return process.exit(1);
}
var FROMDIR = null;
if (args[0] === "-d") {
  FROMDIR = args[1];
  args.splice(0, 2); // git rid of -d fromdir
}
var OUTFILE = null;
if (args[0] === "-o") {
  OUTFILE = args[1];
  args.splice(0, 2); // git rid of -o outfile
}

// var errors = 0;
// var WARN = args[0] === "-w" ? "warn" : args[0] === "-e" ? "err" : null;
// function report (msg) {
//   console.warn(msg);
//   if (WARN === "err") {
//     ++errors;
//   }
// }

var corpus = fs.readdirSync(FROMDIR).filter(fn => {
  // console.warn(path.extname(fn) === ".json", path.relative(".", path.resolve(FROMDIR, fn)), path.relative(".", OUTFILE));
  // old strategy: look for all json files !== to the out corpus
  // return path.extname(fn) === ".json" &&
  //   path.relative(".", path.resolve(FROMDIR, fn)) !== path.relative(".", OUTFILE);
  // new strategy: look for a .shex files
  return path.extname(fn) === ".shex"
}).map(fn => {
  var stem = path.basename(fn, path.extname(fn));
  // var rel = path.relative(".", path.join(path.dirname(fn), base));
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
// console.warn(files);
fs.writeFileSync(OUTFILE, JSON.stringify(corpus, null, "  "), {encoding: "utf8"});
