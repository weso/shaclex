# shaclex

SHACL and SHEX Implementation.

This project contains an implementation of
[SHACL](http://w3c.github.io/data-shapes/shacl/) and
[ShEx](http://www.shex.io)

[![CircleCI](https://circleci.com/gh/labra/shaclex.svg?style=svg)](https://circleci.com/gh/labra/shaclex)
[![Build Status](https://travis-ci.org/labra/shaclex.svg?branch=master)](https://travis-ci.org/labra/shaclex)
[![Codacy Badge](https://api.codacy.com/project/badge/grade/f87bd2ebcfa94dce89e2a981ff13decd)](https://www.codacy.com/app/jelabra/shaclex)

## Introduction

This project contains an implementation of [SHACL](https://www.w3.org/TR/shacl/) and [ShEx](http://shex.io/).

Both are implemented in Scala using the same underlying mechanism using a purely functional approach.
 
The library handles RDF using a [simple RDF interface](https://github.com/labra/shaclex/tree/master/modules/srdf) which has 2 implementations, 
one using [Apache Jena](https://jena.apache.org/) and another one using [RDF4j](http://rdf4j.org/), this means
that it is possible to use this library to validate RDF models from any of those RDF libraries, as well as 
from external SPARQL endpoints.
  

## Installation and compilation

The projects uses [sbt](http://www.scala-sbt.org/) for compilation as well as Java 1.8. 

* `sbt test` compiles and runs the tests


## Usage

Once compiled, the program can be run as a command line tool.
It is possible to run the program inside `sbt` as:

### Validating RDF data with SHACL 

Example:

```
sbt "run -d examples/shacl/good1.ttl --engine ShaClex"
```

### Validating RDF with ShEx 

Example:

```
sbt "run -e ShEx -s examples/shex/good1.shex --schemaFormat ShExC -d examples/shex/good1.ttl"
```

### Interactive mode with `sbt` 

It is usually faster to run the `sbt` command, which opens the interactive `sbt` shell and inside that shell, execute 
the different commands. 

```
$ sbt
... several information about loading libraries
sbt> run -d examples/shacl/good1.ttl --engine ShaClex  
```

### Binary mode

The fastest way to run Shaclex is to compile the code and generate a binary. 
The following command:

```
$ sbt universal:packageBin  
```
 
generates the file:
 
```
target/universal/shaclex-N.N.N.zip
``` 

which contains the compressed binary code. 

### Server mode

The `--server` option activates server mode.

```
sbt "run --server"
```

## Implementation details

* The engine is based on Monads using the [cats library](http://typelevel.org/cats/)
* The ShEx compact syntax parser  
  is implemented using the following [Antlr grammar](https://github.com/shexSpec/grammar/blob/master/ShExDoc.g4) (previous versions used Scala Parser Combinators)
  which is based on this [grammar](https://github.com/shexSpec/shex.js/blob/master/doc/bnf)
* JSON encoding and decoding uses the Json structure [defined here](https://shexspec.github.io/spec/) and is implemented using [Circe](https://github.com/travisbrown/circe)  

## Compatibility tests

We aim to pass the standard test-suites of [ShEx](https://github.com/shexSpec/shexTest) 
and [SHACL](https://w3c.github.io/data-shapes/data-shapes-test-suite/).

In order to run the test suite and generate the EARL report, you can do the following:

- For ShEx, run `sbt`, select `project shex` and run `compat:test` for compatibility tests. 
  

## More information

This project is a based on [ShExcala](http://labra.github.io/ShExcala/) which was focused on Shape Expressions only. 
The aim of Shaclex is to support both ShEx and SHACL and to provide conversions between both languages.
 
## Author & contributors

* Author: [Jose Emilio Labra Gayo](http://www.di.uniovi.es/~labra)

Contributors: 

* [Eric Prud'hommeaux](https://www.w3.org/People/Eric/)
* [Bogdan Roman](https://github.com/bogdanromanx)
* [Andrew Berezovskyi](https://github.com/berezovskyi)

## Contribution

Contributions are greatly appreciated. 
Please fork this repository and open a
pull request to add more features or [submit issues](https://github.com/labra/shaclex/issues)
