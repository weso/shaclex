# shaclex

RDF Data Shapes Implementation. This project contains an implementation of
[SHACL](http://w3c.github.io/data-shapes/shacl/) and of
[ShEx](http://www.shex.io)


[![Codacy Badge](https://api.codacy.com/project/badge/grade/f87bd2ebcfa94dce89e2a981ff13decd)](https://www.codacy.com/app/jelabra/shaclex)
[![Build Status](https://travis-ci.org/labra/shaclex.svg?branch=master)](https://travis-ci.org/labra/shaclex)

## Introduction

This project contains an implementation of SHACL and ShEx. 

Both are implemented using the same underlying mechanism which is based
 on a purely functional approach.

## Installation and compilation

The projects uses [sbt](http://www.scala-sbt.org/) for compilation.

* `sbt test` compiles and runs the tests


## Usage

Once compiled, the program can be run as a command line tool. 
It is possible to run the program inside `sbt` as:

Validates using SHACL (default engine)
```
sbt run -d examples/good1.ttl
```

```
sbt run -e ShEx -s examples/shex/good1.shex --schemaFormat ShExC -d examples/shex/good1.ttl
```

## Implementation details

* The engine is based on Monads
* The Compact syntax parser  
  is implemented using the following [Antlr grammar](https://github.com/shexSpec/grammar/blob/master/ShExDoc.g4) (previous versions used Scala Parser Combinators)
  which is based on this [grammar](https://github.com/shexSpec/shex.js/blob/master/doc/bnf)
* JSON encoding and decoding uses the Json structure [defined here](https://shexspec.github.io/spec/) and is implemented using [Circe](https://github.com/travisbrown/circe)  

## More information

* [ShExcala](http://labra.github.io/ShExcala/): Previous Shape Expressions implementation

## Author

* [Jose Emilio Labra Gayo](http://www.di.uniovi.es/~labra)



