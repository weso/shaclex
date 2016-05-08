package es.weso.validating
import Response._
import org.scalatest._
import cats.implicits._

class ResponseTest extends FunSpec with Matchers with OptionValues {

  describe("Response") {
   
   describe("merge") {
    it("can merge a response with a seq of responses") {
      val r1: Response[Int,Option] = Response(Some(1))
      val rs: Response[Seq[Int],Option] = Response(Some(Seq(2,3)))
      val expected: Response[Seq[Int],Option] = Response(Some(Seq(1,2,3)))
      r1.merge(rs) should be(expected)
    }
   }
   
   describe("merge") {
    it("can add two seq of responses") {
      val r1: Response[Seq[Int],Option] = Response(Some(Seq(1,2)))
      val r2: Response[Seq[Int],Option] = Response(Some(Seq(3,4)))
      val expected: Response[Seq[Int],Option] = Response(Some(Seq(1,2,3,4)))
      add(r1,r2) should be(expected)
    }
   }
   
  }  
}
