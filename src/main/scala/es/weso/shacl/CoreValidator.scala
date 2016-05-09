package es.weso.shacl

import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.validating.{
  Constraint => VConstraint, 
  _
}
import ViolationError._
import Validated._
import ConstraintReason._
import org.apache.log4j._

/**
 * This validator will be implemented directly in Scala
 */
case class CoreValidator(schema: Schema) {

  type ValidationResult = Validated[Schema,ConstraintReason,Throwable]
  type ShapeConstraint = VConstraint[RDFReader,Shape,Throwable]
  type SchemaConstraint = VConstraint[RDFReader,Schema,Throwable]
  type NodeShapeConstraint = VConstraint[RDFReader,(RDFNode,Shape),Throwable]
  type RDFNodeConstraint = VConstraint[RDFReader, RDFNode, Throwable]
  type VPropertyConstraint = VConstraint[(RDFReader,IRI), RDFNode, Throwable]
  type Explain = String

  lazy val log = LogManager.getRootLogger
  
  /**
   * Return all scopeNode declarations which are pairs (n,s) where
   * <p> `n` = node to validate
   * <p> `s` = candidate shape
   */
  def scopeNodes: Seq[(IRI,Shape)] = {
    schema.scopeNodeShapes
  }
  
  def validate(rdf: RDFReader):
      Validated[Schema,ConstraintReason,Throwable] = {
    schemaConstraint.validate(schema,rdf)
  }
  
  def schemaConstraint: SchemaConstraint = Single((schema,rdf) => {
    val shapes = schema.shapes
    log.info(s"schemaConstraint, shapes: $shapes")
    log.info(s"schemaConstraint, rdf: $rdf")
    val results = shapes.map(sh => shapeConstraint.validate(sh,rdf))
    val result = all(results)
    result.mapValue(_ => schema)
  })

  def shapeConstraint: ShapeConstraint = Single((shape,rdf) => {
    val nodes = shape.scopeNodes
    log.info(s"schemaConstraint, shape: $shape")
    val results = nodes.map(n => shapeNodeConstraint.validate((n,shape),rdf))
    val result = all(results)
    result.mapValue(_ => shape)  
  })
  
  def shapeNodeConstraint: NodeShapeConstraint = Single((pair,rdf) => {
    val (node,shape) = pair
    log.info(s"node $node, shape: $shape")
    val cs = shape.components.map(vConstraint)
    val c = All(cs) 
    val result = c.validate(node,rdf) 
    log.info(s"result $result, cs: $cs")
    result.mapValue(n => (n,shape)) 
  })
  
  def vConstraint(c:Constraint): RDFNodeConstraint = {
    c match {
      case pc:PropertyConstraint => vPropertyConstraint(pc)
      case _ => undef("vConstraint " + c)
    }
  }
  
  def undef[A](msg:String): VConstraint[A,RDFNode,Throwable] = {
    VConstraint.unsupported[A, RDFNode, ConstraintReason,Throwable](msg)
  }
  
  def vPropertyConstraint(pc: PropertyConstraint): 
       RDFNodeConstraint = Single((node,rdf) => {
    val predicate = pc.predicate
    val components = pc.components
    val pcConstraints = components.map(component2VPropertyConstraint(predicate))
    val result : VPropertyConstraint = All(pcConstraints)
    result.validate(node, (rdf,predicate))
  })
  
  def component2VPropertyConstraint(p:IRI)(c: PCComponent): VPropertyConstraint = {
    c match {
      case MinCount(n) => minCount(n)
      case MaxCount(n) => maxCount(n)
      case _ => undef("Unsupported component: " + c.toString)
    }
  }

  
  def explain(message: String) = message 
  
  def minCount(minCount: Int): VPropertyConstraint = Single((node,ctx) => {
    val (rdf,predicate) = ctx
    val count = rdf.triplesWithSubjectPredicate(node,predicate).size
    log.info(s"minCount: node $node, predicate: $predicate $count $minCount")
    if (count < minCount) {
      err(minCountError(node,predicate,minCount,count))
    }
    else ok(singleReason(node,
      s"$node satisfies minCount=$minCount for predicate $predicate with count=$count"))
   })
   
  def maxCount(maxCount: Int): VPropertyConstraint = Single((node,ctx) => {
    val (rdf,predicate) = ctx
    val count = rdf.triplesWithSubjectPredicate(node,predicate).size
    if (count > maxCount) {
      err(maxCountError(node,predicate,maxCount,count))
    }
    else ok(singleReason(node,
      explain(s"$node satisfies maxCount=$maxCount for predicate $predicate with count=$count")))
   })
  
  def showConstraintReason[A](cr: ConstraintReason[A]): String = {
    cr match {
      case SingleReason(_,msg) => msg
      case AllReason(cs) => "all[" + cs.map(c => showConstraintReason(c) + "\n") + "]"
      case SomeReason(cs) => "some[" + cs.map(c => showConstraintReason(c) + "\n") + "]"
      case OneOfReason(c) => "oneOf[" + showConstraintReason(c) + "]"
    }
  }
   
  def showResponse(r: Response[Schema,ConstraintReason]): String = {
    showConstraintReason(r.response) 
  }
   
  def showResult(r: ValidationResult): String = {
   val sb = new StringBuilder
   if (r.isOK) {
     sb ++= "Validated"
     val rs = r.reasons.get
     rs.values.size match {
       case 0 => sb ++= "(no response)"
       case 1 => sb ++= "Response:\n" ++ showResponse(rs.values.head)
       case _ => {
         sb ++= "Several responses:\n" 
         for (r <- rs.values) {
           sb ++= showResponse(r) + "\n"
         }
       }
     }
   } else {
     sb ++= "Not valid. Errors:\n"
     for (e <- r.errors) {
       sb ++= "Error: " ++ e.getMessage + "\n"
     }
   }
   sb.toString
  }
   
}

object CoreValidator {
  def empty = CoreValidator(schema = Schema.empty)
  
}

