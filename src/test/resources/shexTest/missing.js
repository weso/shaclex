#!/usr/bin/env node
// test for schemas referenced by validation/manifestTest.ttl that aren't in schemas/manifestTest.ttl
// ./missing.js OR node ./missing.js
// empty array ([]) means nothing to do

let fs = require('fs')
let s = JSON.parse(fs.readFileSync('schemas/manifestTest.jsonld', 'utf-8'))
['@graph'][0]
  .entries.map(e => e.shex)
let v = JSON.parse(fs.readFileSync('validation/manifestTest.jsonld', 'utf-8'))
['@graph'][0].entries
  .map(e => e.action.schema)
  .filter(e => e.startsWith('../schemas/'))
  .map(e => e.substr('../schemas/'.length));
let v2 = v.filter((e, i) => v.lastIndexOf(e) === i);
let x = v2.filter(e => s.indexOf(e) === -1)
console.log(x)
