package es.weso.validating
import org.scalatest._

class ResponseTest extends FunSpec with Matchers with OptionValues {

 describe("Response") {
   val r1: Response[Int,Option[Int]] = Response(1,Some(1))
   r1.map2(_+1,_.get).value should be(2)
 }  
}
