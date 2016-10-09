package es.weso.validating
import Constraint._
import org.scalatest._
import cats.implicits._

class CheckedValTest 
 extends FunSpec with Matchers with OptionValues {
  
/*  describe("Checked values") {
    it("Should check if they are ok") { 
      val x : CheckedVal[Int,List[Int]] = okSingle(2,List(2),"even")
      x.isOK should be(true)
    }
    
    it("Should check if and of two oks is ok") { 
      val ok1 : CheckedVal[Int,List[Int]] = okSingle(2,List(2),"even")
      val ok2 : CheckedVal[Int,List[Int]] = okSingle(3,List(3),"odd")
      val r = Constraint.all(Seq(ok1,ok2))
      r.isOK should be(true)
    }
  }
*/  
}


