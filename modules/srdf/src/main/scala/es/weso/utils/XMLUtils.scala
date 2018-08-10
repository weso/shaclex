package es.weso.utils

import java.util.GregorianCalendar

import cats.implicits._
import javax.xml.datatype.{DatatypeConstants, DatatypeFactory, XMLGregorianCalendar}

import scala.util.{Either, Try}


object XMLUtils {

  def lessThanXSDDateTimes(dateTime1: String, dateTime2: String): Either[String,Boolean] = for {
    d1 <- xsdDatetime2GregorianCalendar(dateTime1)
    d2 <- xsdDatetime2GregorianCalendar(dateTime2)
  } yield {
  //    val r1 = d1.toZonedDateTime.toLocalDateTime
  //   val r2 = d2.toZonedDateTime.toLocalDateTime
  //  r1.isBefore(r2)
    d1.compare(d2) == DatatypeConstants.LESSER
  }


  def xsdDatetime2GregorianCalendar(s: String): Either[String, XMLGregorianCalendar] =
    Try {
      DatatypeFactory.newInstance.newXMLGregorianCalendar(s)
    }.toEither.leftMap(e =>
      s"Error parsing ${s} as XSD Datetime: ${e.getMessage}"
    )
}