package es.weso.shex.validator

import es.weso.rdf.nodes.RDFNode
import org.apache.xerces.impl.dv.{SchemaDVFactory, ValidatedInfo, XSSimpleType}
import org.apache.xerces.impl.dv.xs.DecimalDV
import org.apache.xerces.impl.validation.ValidationState

import scala.util._

object NodeInfo {

  /* This implementation leverages on Xerces internal implementation of XML Schema datatypes */
  /* This is probably going too far and could be simplified */
  def totalDigits(node: RDFNode): Int = {
   val t = Try {
      val context = new ValidationState
      val decimalDV = new DecimalDV()
      val typeDeclaration: XSSimpleType = SchemaDVFactory.getInstance.getBuiltInType("decimal")
      val resultInfo = new ValidatedInfo
      typeDeclaration.validate(node.getLexicalForm, context, resultInfo)
      decimalDV.getTotalDigits(resultInfo.actualValue)
    }
    t match {
      case Failure(e) => 0
      case Success(n) => n
    }
  }

  /* This implementation leverages on Xerces internal implementation of XML Schema datatypes */
  /* This is probably going too far and could be simplified */
  def fractionDigits(node: RDFNode): Int = {
    val t = Try {
      val context = new ValidationState
      val decimalDV = new DecimalDV()
      val typeDeclaration: XSSimpleType = SchemaDVFactory.getInstance.getBuiltInType("decimal")
      val resultInfo = new ValidatedInfo
      typeDeclaration.validate(node.getLexicalForm, context, resultInfo)
      decimalDV.getFractionDigits(resultInfo.actualValue)
    }
    t match {
      case Failure(e) => 0
      case Success(n) => n
    }
  }

  def length(node: RDFNode): Int = node.getLexicalForm.length

}