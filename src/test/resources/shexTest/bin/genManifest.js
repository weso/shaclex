#!/usr/bin/env node

/* genManifest - tool to generate manifest.ttl for syntax and structure tests
 * install and run:

git clone git@github.com:shexSpec/shexTest.git
npm install
cd negativeSyntax/
../bin/genManifest.js NegativeSyntax

*/

/*global process, console, require, __dirname*/
// Parse arguments
var args = process.argv.slice(2);
if (args > 1 || args.indexOf("-help") !== -1 || args.indexOf("--help") !== -1) {
  console.error('usage: genManifest TestClass > manifest.ttl');
  process.exit(1);
}

var testClass = args[0] || "NegativeSyntax";
var comment = {
  "NegativeSyntax": "ShEx negative syntax tests",
  "NegativeStructure": "ShEx negative structure tests"
}[testClass];
var fs = require('fs');
var path = require("path");

var testDir = path.basename(process.cwd());
var basePath = "https://raw.githubusercontent.com/shexSpec/shexTest/master/";
var dirPath = basePath + testDir + '/';

// Output preamble
console.log('@base <https://raw.githubusercontent.com/shexSpec/shexTest/master/' + testDir + '/manifest> .\n\
@prefix rdf:    <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n\
@prefix rdfs:   <http://www.w3.org/2000/01/rdf-schema#> .\n\
@prefix mf:     <http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#> .\n\
@prefix sht:    <http://www.w3.org/ns/shacl/test-suite#> .\n\
@prefix sx:     <https://shexspec.github.io/shexTest/ns#> .\n\
\n\
<> a mf:Manifest ;\n\
    rdfs:comment "' + comment + '" ;\n\
    mf:entries (');

// Get .shex files for generating identifiers
fs.readdir(process.cwd(), function(err, files) {
  if (err) {
    console.error('Error reading directory ' + process.cwd());
    console.error(err);
    return;
  }
  var shexs = files.filter(function(file) {return file.endsWith('.shex');});

  // Create entries
  shexs.forEach(function(file) {
    console.log('    <#' + path.basename(file, '.shex') + '>');
  });
  console.log("  ) .\n");

  // Create each test
  shexs.forEach(function(file) {
    var id = path.basename(file, '.shex');
    console.log('<#' + id + '> a sht:' + testClass + ' ;');
    console.log('  mf:name "' + id + '" ;');
    console.log('  mf:status mf:proposed ;');
    console.log('  sx:shex <' + file + '> ;');
    console.log('  .\n');
  });
});
