# shaclex

SHACL and SHEX Implementation.

This project contains an implementation of
[SHACL](http://w3c.github.io/data-shapes/shacl/) and
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

## More information

This project is a continuation of [ShExcala](http://labra.github.io/ShExcala/) which was focused on Shape Expressions only. In this project the underlying validation computation is based on Monad transformers.

## Author

* [Jose Emilio Labra Gayo](http://www.di.uniovi.es/~labra)

## Contribution

Contributions are greatly appreciated. Please fork this repository and open a
pull request to add more features or [submit issues](https://github.com/labra/shaclex/issues)
