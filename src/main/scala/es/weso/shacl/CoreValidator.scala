package es.weso.shacl

import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.validating.{
  Constraint => VConstraint, 
  _
}
import ViolationError._
import Checked._
import ConstraintReason._
import org.apache.log4j._

/**
 * This validator is implemented directly in Scala
 */
case class CoreValidator(schema: Schema) {

 type ValidationResult = Checked[Schema,ConstraintReason,ConstraintError]
 type ShapeConstraint = VConstraint[RDFReader,Shape,ConstraintError]
 type SchemaConstraint = VConstraint[RDFReader,Schema,ConstraintError]
 type NodeShapeConstraint = VConstraint[RDFReader,(RDFNode,Shape),ConstraintError]
 type RDFNodeConstraint = VConstraint[RDFReader, RDFNode, ConstraintError]
 type VPropertyConstraint = VConstraint[(RDFReader,IRI), RDFNode, ConstraintError]
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
      Checked[Schema,ConstraintReason,ConstraintError] = {
    schemaConstraint.validate(schema,rdf)
  }
  
  
  def schemaConstraint: SchemaConstraint = Single((schema,rdf) => {
    val shapes = schema.shapes
    log.info(s"schemaConstraint, shapes: $shapes")
    log.info(s"schemaConstraint, rdf: $rdf")
    val results: Seq[Checked[Shape, ConstraintReason, ConstraintError]] = 
      shapes.map(sh => shapeConstraint.validate(sh,rdf))
    val result: Checked[Seq[Shape], ConstraintReason, ConstraintError] = checkAll(results)
    result.mapValue(_ => schema)
  }) 

  def shapeConstraint: ShapeConstraint = Single((shape,rdf) => {
    val nodes = shape.scopeNodes
    log.info(s"schemaConstraint, shape: $shape")
    val results = nodes.map(n => shapeNodeConstraint.validate((n,shape),rdf))
    val result : Checked[Seq[(RDFNode,Shape)], ConstraintReason, ConstraintError] = checkAll(results)
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
  
  def undef[A](msg:String): VConstraint[A,RDFNode,ConstraintError] = {
    VConstraint.unsupported[A, RDFNode, ConstraintReason](msg)
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
      checkError(minCountError(node,predicate,minCount,count))
    }
    else ok(singleReason(node,
      s"$node satisfies minCount=$minCount for predicate $predicate with count=$count"))
   }) 
   
  def maxCount(maxCount: Int): VPropertyConstraint = Single((node,ctx) => {
    val (rdf,predicate) = ctx
    val count = rdf.triplesWithSubjectPredicate(node,predicate).size
    if (count > maxCount) {
      checkError(maxCountError(node,predicate,maxCount,count))
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
     sb ++= "Checked"
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
       sb ++= "Error: " ++ e.toString + "\n"
     }
   }
   sb.toString
  }
  
}

object CoreValidator {
  def empty = CoreValidator(schema = Schema.empty)
  
}

