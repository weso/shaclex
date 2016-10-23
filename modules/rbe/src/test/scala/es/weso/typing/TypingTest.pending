package es.weso.typing

import org.scalatest._
import es.weso.collection._
import es.weso.rbe.interval.Interval._
import util.Success

class TypingTest extends FunSpec with Matchers with TryValues {

  describe("Typings") {
    
     val t : PosNegTyping[String,String] = 
        PosNegTyping.fromPosMap(Map("n1" -> Set("t1","t2"), "n2" -> Set("t1","t3")))
    
     it("should add a typing to an element that doesn't exist") {
      t.addPosType("n3","t2") should be(Success(PosNegTyping.fromPosMap(
          Map("n1" -> Set("t1","t2"), "n2" -> Set("t1","t3"), "n3" -> Set("t2"))
          )))
    }
    
     it("should add a negative typing to an element that doesn't exist") {
      t.addNegType("n3","t2") should be(Success(PosNegTyping.fromPosNegMap(
          Map("n1" -> (Set("t1","t2"),Set[String]()), 
              "n2" -> (Set("t1","t3"),Set[String]()), 
              "n3" -> (Set[String](),Set("t2")))
          )))
    }
     
    it("should not add a negative typing to an element that exist with positive typing") {
      t.addNegType("n2","t3").failure 
    }
  
    it("should add a negative typing to an element that exist with a different positive typing") {
      t.addNegType("n2","t4").success 
    }
  }
  
  describe("Combine typings") {
    
    it("Should combine two typings when the first is empty") {
      val t1 : PosNegTyping[String,String] = 
        PosNegTyping.empty
      val t2 : PosNegTyping[String,String] = 
        PosNegTyping.fromPosMap(Map("n3" -> Set("t1","t2"), "n4" -> Set("t4"))) 
      val expected = t2
      t1.combine(t2) should be(Success(expected))      
   }
    it("Should combine two typings when the second is empty") {
      val t1 : PosNegTyping[String,String] = 
        PosNegTyping.empty
      val t2 : PosNegTyping[String,String] = 
        PosNegTyping.fromPosMap(Map("n3" -> Set("t1","t2"), "n4" -> Set("t4"))) 
      val expected = t2
      t2.combine(t1) should be(Success(expected))      
   }
    it("Should combine two typings with disjoint pos and negs") {
      val t1 : PosNegTyping[String,String] = 
        PosNegTyping.fromPosMap(Map("n1" -> Set("t1","t2"), "n2" -> Set("t1","t3")))
      val t2 : PosNegTyping[String,String] = 
        PosNegTyping.fromPosMap(Map("n3" -> Set("t1","t2"), "n4" -> Set("t4"))) 
      val expected = 
        PosNegTyping.fromPosMap(Map(
            "n1" -> Set("t1","t2"), 
            "n2" -> Set("t1","t3"), 
            "n3" -> Set("t1","t2"), 
            "n4" -> Set("t4")))
      t1.combine(t2) should be(Success(expected))      
   }
   it("Should combine two typings with overlapping pos 1") {
      val t1 : PosNegTyping[String,String] = 
        PosNegTyping.fromPosMap(Map("n1" -> Set("t1")))
      val t2 : PosNegTyping[String,String] = 
        PosNegTyping.fromPosMap(Map("n1" -> Set("t1"))) 
      val expected = 
        PosNegTyping.fromPosMap(Map(
            "n1" -> Set("t1")))
      t1.combine(t2) should be(Success(expected))      
   }

    it("Should combine two typings with overlapping pos") {
      val t1 : PosNegTyping[String,String] = 
        PosNegTyping.fromPosMap(Map("n1" -> Set("t1","t2"), "n2" -> Set("t1","t3")))
      val t2 : PosNegTyping[String,String] = 
        PosNegTyping.fromPosMap(Map("n1" -> Set("t1","t3","t5"), "n4" -> Set("t4"))) 
      val expected = 
        PosNegTyping.fromPosMap(Map(
            "n1" -> Set("t1","t2","t3","t5"), 
            "n2" -> Set("t1","t3"), 
            "n4" -> Set("t4")))
      t1.combine(t2) should be(Success(expected))      
   }
    
   it("Should fail to combine two typings with overlapping pos and neg") {
      val t1 : PosNegTyping[String,String] = 
        PosNegTyping.fromPosMap(Map("n1" -> Set("t1","t2"), "n2" -> Set("t1","t3")))
      val t2 : PosNegTyping[String,String] = 
        PosNegTyping.fromPosMap(Map("n1" -> Set("t1","t3","t5"), "n4" -> Set("t4")))
      val t3 = t2.addNegType("n2", "t3").get
      t1.combine(t3).failure      
   }
   it("Should not fail to combine two typings with overlapping pos and neg but different values") {
      val t1 : PosNegTyping[String,String] = 
        PosNegTyping.fromPosMap(Map("n1" -> Set("t1","t2"), "n2" -> Set("t1","t3")))
      val t2 : PosNegTyping[String,String] = 
        PosNegTyping.fromPosMap(Map("n1" -> Set("t1","t3","t5"), "n4" -> Set("t4")))
      val t3 = t2.addNegType("n2", "t5").get
      val e1 = 
        PosNegTyping.fromPosMap(Map(
            "n1" -> Set("t1","t2","t3","t5"), 
            "n2" -> Set("t1","t3"), 
            "n4" -> Set("t4")))
      val expected = e1.addNegType("n2", "t5").get      
      t1.combine(t3) should be(Success(expected))      
   }
 
  }
}