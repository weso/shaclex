package es.weso.shextest.manifest

import es.weso.rdf.nodes._
import ManifestPrefixes._

abstract trait Entry {
    def node: RDFNode
    def entryType: IRI
    def status: Status
    def name: String
  }
  
case class RepresentationTest(override val node: RDFNode,
                                override val status: Status,
                                override val name: String,
                                json: IRI,
                                shex: IRI,
                                ttl: IRI) extends Entry {
    override val entryType = sht_RepresentationTest
  }
  
case class Validate(override val node: RDFNode,
                      override val status: Status,
                      override val name: String,
                      action: ManifestAction,
                      result: Result,
                      specRef: Option[IRI]
                     ) extends Entry {
    override val entryType = sht_Validate
  }
  
  abstract class ValidOrFailureTest(override val node: RDFNode,
                                override val status: Status,
                                override val name: String,
                                val traits: List[IRI],
                                val comment: String,
                                val action: Action,
                                val maybeResult: Option[IRI],
                                val entryType : IRI
                                ) extends Entry {
  }
  
  case class ValidationTest(override val node: RDFNode,
                      override val status: Status,
                      override val name: String,
                      override val traits: List[IRI],
                      override val comment: String,
                      override val action: Action,
                      override val maybeResult: Option[IRI]
                     ) extends
    ValidOrFailureTest(node,status,name,traits,comment,action,maybeResult, sht_ValidationTest) {
  }
  
  case class ValidationFailure(override val node: RDFNode,
                               override val status: Status,
                               override val name: String,
                               override val traits: List[IRI],
                               override val comment: String,
                               override val action: Action,
                               override val maybeResult: Option[IRI]
                              ) extends
    ValidOrFailureTest(node,status,name,traits,comment,action,maybeResult,sht_Validate) {
  }
  
  case class NegativeSyntax(override val node: RDFNode,
                            override val status: Status,
                            override val name: String,
                            shex: IRI
                           ) extends Entry {
    override val entryType = sht_NegativeSyntax
  }
  
  case class NegativeStructure(override val node: RDFNode,
                            override val status: Status,
                            override val name: String,
                            shex: IRI
                           ) extends Entry {
    override val entryType = sht_NegativeStructure
  }
  