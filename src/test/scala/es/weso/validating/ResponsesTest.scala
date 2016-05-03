package es.weso.validating
import Responses._
import org.scalatest._
import cats.implicits._

class ResponsesTest extends FunSpec with Matchers with OptionValues {

  describe("Responses") {
    
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
  

}
