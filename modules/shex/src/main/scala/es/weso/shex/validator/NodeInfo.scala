package es.weso.shex.validator

import es.weso.rdf.nodes.{Literal, RDFNode}
import es.weso.rdf.PREFIXES._
import es.weso.rdf.RDFReader
import org.apache.xerces.impl.dv.{SchemaDVFactory, ValidatedInfo, XSSimpleType}
import org.apache.xerces.impl.dv.xs.DecimalDV
import org.apache.xerces.impl.validation.ValidationState

import scala.util._

object NodeInfo {

  /* This implementation leverages on Xerces internal implementation of XML Schema datatypes */
  /* This is probably going too far and could be simplified */
  def totalDigits(node: RDFNode, rdf: RDFReader): Either[String,Int] = {
    node match {
      case l: Literal => l.dataType match {
        case `xsd_decimal` | `xsd_integer` => for {
          b <-rdf.checkDatatype(node,l.dataType)
          td <- {
            val t = Try {
              val context                       = new ValidationState
              val decimalDV                     = new DecimalDV()
              val typeDeclaration: XSSimpleType = SchemaDVFactory.getInstance.getBuiltInType("decimal")
              val resultInfo                    = new ValidatedInfo
              typeDeclaration.validate(node.getLexicalForm, context, resultInfo)
              decimalDV.getTotalDigits(resultInfo.actualValue)
            }
            t match {
              case Failure(e) => Left(s"Error calculating totalDigits of $node: ${e.getMessage}")
              case Success(n) => Right(n)
            }
          }
        } yield td
        case d => Left(s"TotalDigits can only be applied to xsd:decimal or derived datatypes, not to: $d")
      }
      case _ => Left(s"TotalDigits facet can not be applied to non literal node: $node")
    }
  }

  /* This implementation leverages on Xerces internal implementation of XML Schema datatypes */
  /* This is probably going too far and could be simplified */
  def fractionDigits(node: RDFNode, rdf: RDFReader): Either[String,Int] = {
    node match {
      case l: Literal =>
        l.dataType match {
          case `xsd_decimal` | `xsd_integer` => {
            rdf.checkDatatype(node,l.dataType).fold(
              e => Left(s"Node $node has wrong datatype"),
              _ => { val t = Try {
              val context                       = new ValidationState
              val decimalDV                     = new DecimalDV()
              val typeDeclaration: XSSimpleType = SchemaDVFactory.getInstance.getBuiltInType("decimal")
              val resultInfo                    = new ValidatedInfo
              typeDeclaration.validate(node.getLexicalForm, context, resultInfo)
              decimalDV.getFractionDigits(resultInfo.actualValue)
            }
            t match {
              case Failure(e) => Left(s"Error calculating fractionDigits of $node: ${e.getMessage}")
              case Success(n) => Right(n)
            }
          })
            }
          case d => Left(s"FractionDigits can only be applied to xsd:decimal or derived datatypes, not to: $d")
        }
      case _ => Left(s"FractionDigits facet can not be applied to non literal node: $node")
    }
  }

  def length(node: RDFNode): Int = node.getLexicalForm.length

}