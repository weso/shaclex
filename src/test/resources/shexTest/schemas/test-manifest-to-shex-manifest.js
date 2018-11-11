#!/usr/bin/env node

fs = require('fs')
let old = JSON.parse(fs.readFileSync('manifest.jsonld', 'utf8'))
let oldEntries = old['@graph'][0].entries
let newEntries = oldEntries.map(
  e => ({
    schemaLabel: e.name,
    shexURL: e.shex,
    jsonURL: e.json,
    rdfURL: e.ttl
  })
)
fs.writeFileSync('representationTests.json', JSON.stringify(newEntries, null, 2), 'utf8')
