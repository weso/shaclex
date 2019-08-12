package es.weso.shacl.report

import org.scalatest.{FunSpec, Matchers}

object Counter {
  var counter = 0
  def increment(): Unit = { counter += 1 }
  def get: Int = counter
}

class CounterTest extends FunSpec with Matchers {

 val counter = Counter

 describe("Global counter") {
   Range(0,5) foreach(i => {
     it(s"Should check $i") {
       if (i % 2 == 0) {
         counter.increment
         info(s"Test $i passed with counter: ${counter.get}")
       } else {
         counter.increment
         info(s"Test $i failed with counter: ${counter.get}")
       }
     }
   })
   it(s"Shows counter") { info(s"Counter: ${counter.get}") }
 }

}