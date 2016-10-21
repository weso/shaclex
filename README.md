# shaclex

RDF Data Shapes Implementation. This project contains an implementation of
[SHACL](http://w3c.github.io/data-shapes/shacl/) and of
[ShEx](http://www.shex.io)


[![Codacy Badge](https://api.codacy.com/project/badge/grade/f87bd2ebcfa94dce89e2a981ff13decd)](https://www.codacy.com/app/jelabra/shaclex)
[![Build Status](https://travis-ci.org/labra/shaclex.svg?branch=master)](https://travis-ci.org/labra/shaclex)

## Introduction

This is an experimental implementation of SHACL and ShEx. Both are implemented using the same underlying mechanism which is based
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
## More information

* [ShExcala](http://labra.github.io/ShExcala/): Previous Shape Expressions implementation

## Author

* [Jose Emilio Labra Gayo](http://www.di.uniovi.es/~labra)



