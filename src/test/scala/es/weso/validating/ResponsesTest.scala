package es.weso.validating
import Responses._
import org.scalatest._
import cats.implicits._

class ResponsesTest extends FunSpec with Matchers with OptionValues {

  describe("Responses") {
   
    describe("combineWith") {
    it("can combine several responses") {
      val r1: Response[Int,Option] = Response(Some(1))
      val r2: Response[Int,Option] = Response(Some(2))
      
      val rs1: Responses[Int,Option] = Responses(Seq(r1,r2))
      val rs2: Responses[Int,Option] = Responses(Seq(Response(Some(3)),Response(Some(4))))

      def f(r1:Response[Int,Option],r2:Response[Int,Option]): Response[Int,Option] = {
        val r = (r1.response,r2.response) match {
          case (None,None) => None
          case (None,Some(x)) => Some(x)
          case (Some(x),None) => Some(x)
          case (Some(x),Some(y)) => Some(x + y)
        }
        Response(r)
      }
     val rs = rs1.combineWith(rs2,f)
     val r4 : Response[Int,Option] = Response(Some(4))
     val r5 : Response[Int,Option] = Response(Some(5))
     val r6 : Response[Int,Option] = Response(Some(6))
     rs.values should contain theSameElementsInOrderAs Seq(r4,r5,r5,r6)
    }
  }
  describe("merge") {
   it("can merge responses") {
     val r1: Responses[Int,Option] = single(Some(1))
     val r23: Responses[Seq[Int],Option] = single(Some(Seq(2,3)))
     val expected: Responses[Seq[Int],Option] = single(Some(Seq(1,2,3)))
     r1.merge(r23) should be(expected)
   }
   it("can merge empty response with a list of responses") {
     val r0: Responses[Int,Option] = initial
     val r23: Responses[Seq[Int],Option] = single(Some(Seq(2,3)))
     val expected: Responses[Seq[Int],Option] = single(Some(Seq(2,3)))
     r0.merge(r23) should be(expected)
   }
  
  }
  describe("add") {
   it("can add responses") {
     val r1: Response[Int,Option] = Response(Some(1))
     val r23: Responses[Seq[Int],Option] = single(Some(Seq(2,3)))
     val expected: Responses[Seq[Int],Option] = single(Some(Seq(1,2,3)))
     add(r1,r23) should be(expected)
   }
  
  }
  }  
}
