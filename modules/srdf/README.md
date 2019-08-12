# Simple RDF

This module represents a generic and simple RDF interface. 
The ShEx/SHACL library uses only methods provided by this interface so it can be used by different libraries.

It defines 3 traits: 
- [RDFReader](https://github.com/labra/shaclex/blob/master/modules/srdf/src/main/scala/es/weso/rdf/RDFReader.scala) declares methods to read data from an RDF model
- [RDFBuilder](https://github.com/labra/shaclex/blob/master/modules/srdf/src/main/scala/es/weso/rdf/RDFBuilder.scala) contains methods to build RDF models. Generate an empty model, add or remove triples.
- [RDFReasoner](https://github.com/labra/shaclex/blob/master/modules/srdf/src/main/scala/es/weso/rdf/RDFReasoner.scala) extends an RDFReader with the possibility of extending the model with the triples inferred by some inference engine.

It also defines the main components of RDF which are:
- [RDFTriple](https://github.com/labra/shaclex/blob/master/modules/srdf/src/main/scala/es/weso/rdf/triples/RDFTriple.scala) declares an RDF triple.
- [RDFNode](https://github.com/labra/shaclex/blob/master/modules/srdf/src/main/scala/es/weso/rdf/nodes/RDFNode.scala) declares RDF nodes (subjects or objects)
- [IRI](https://github.com/labra/shaclex/blob/master/modules/srdf/src/main/scala/es/weso/rdf/nodes/IRI.scala) declares resources which are IRIs 
- [BNode]((https://github.com/labra/shaclex/blob/master/modules/srdf/src/main/scala/es/weso/rdf/nodes/BNode.scala)) declares Blank nodes 
- [Literal](https://github.com/labra/shaclex/blob/master/modules/srdf/src/main/scala/es/weso/rdf/nodes/Literal.scala) declares RDF literals
- [Lang](https://github.com/labra/shaclex/blob/master/modules/srdf/src/main/scala/es/weso/rdf/nodes/Lang.scala) handles Languages

Some utilities to work with RDF are:

- [Prefixes](https://github.com/labra/shaclex/blob/master/modules/srdf/src/main/scala/es/weso/rdf/PrefixMap.scala) declares a prefix map (a map associating aliases with IRIs)
- [RDFParser](https://github.com/labra/shaclex/blob/master/modules/srdf/src/main/scala/es/weso/rdf/parser/RDFParser.scala) a reader monad to parse RDF with some common utilities
- [SHACLPath](https://github.com/labra/shaclex/blob/master/modules/srdf/src/main/scala/es/weso/rdf/path/SHACLPath.scala) declares SHACL paths

This interface has two implementations:

- [SRDFJena](https://github.com/labra/shaclex/tree/master/modules/srdfJena) for [Apache Jena](https://jena.apache.org/)
- [SRDF4j](https://github.com/labra/shaclex/tree/master/modules/srdf4j) for [RDF4j](http://rdf4j.org/)
