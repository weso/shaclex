package es.weso.validating

import org.scalactic._
import es.weso.generic._

class ValidatingError(msg:String) extends Exception(msg)

case class MsgError(msg:String) extends ValidatingError(msg)
case class OneOfWithSeveralValid[A,R](valids: Responses[A,R]) extends ValidatingError("More than one alternative in oneOf is valid")
case object NoneValid extends ValidatingError("None Valid")
case class Unsupported(msg:String) extends ValidatingError(msg)

/**
 * Represents a deterministic response which contains a single value and reason
 * @tparam A value of the response
 * @tparam R reason 
 */
case class Response[+A,+R](value: A, reason: R) {
  
  def mapValue[B](f: A => B): Response[B,R] = {
    Response(f(value),reason)
  }
  
  override def toString: String = {
    "[" + value + ": " + reason + "]"
  }
}

/**
 * Represents a non-deterministic response which can contains several 
 * values and reasons
 */
case class Responses[+A,+R](values: Seq[Response[A,R]]) {
  
  def combine[B, R1 >: R](other:Responses[B,R1])(f: A => B): Responses[B,R1] = {
    ???
  }
  
  /**
   * Concatenate two responses appending the values and reasons
   */
  def ++[A1 >: A,R1 >: R](other: Responses[A1,R1]): Responses[A1,R1] = {
    Responses(values ++ other.values)
  }
  
  def map[B](f: A => B): Responses[B,R] = {
    Responses(values.map(r => r.mapValue(f)))
  }
  
  override def toString: String = {
    values.toString
  }
}

object Responses {
  
  def single[A,R](a:A, r:R): Responses[A,R] = {
    Responses(Seq(Response(a,r)))
  }
  
  def empty = Responses(Seq())
}

/**
 * Validated defines values that have been validated
 * 
 * @tparam A type of values
 * @tparam R Explanation type of validation
 * @tparam E Error type
 */
case class Validated[+A,+R,+E >: Throwable] private(value: Responses[A,R] Or Every[E]){
  
import Validated._
  
 /**
   * @return `true` if the value has no errors
   */
  def isOK: Boolean = value.isGood

  
 /**
   * Fold a validated value into another value
   * @tparam V the result type 
   * @param ok function to apply to the value when it is OK
   * @param err function to apply to the sequence of errors
   * @return the result of applying either the ok function or the err function 
   */
  def fold[V](ok:Responses[A,R] => V, err: Seq[E] => V): V = {
     value.fold(ok,(es: Every[E]) => err(es.toSeq))   
  }
  
  /**
   * Build a new Validated value by applying a function to the values and reasons
   * @tparam B the value type of the new Validated
   * @param f the function to apply to the current value and reason
   * @return a new Validated with the new value or the list of existing errors
   */
  def map[B, R1 >: R](f: Responses[A,R1] => Responses[B,R1]): Validated[B,R1,E] = {
    Validated(value.map(f))
  }
  
  /**
   * Build a new Validated value by applying a function to the value. It lets the reasons untouched
   * @tparam B the value type of the new Checker
   * @param f the function to apply to the possible values
   * @return a new Validated with the new value or the list of existing errors
   */
  def mapValue[B](f: A => B): Validated[B,R,E] = {
    Validated(value.map(rs => rs.map(f)))
  }
  
 /**
   * Adds an Error to a Checker.
   * @param e error to add
   * @return If the value was ok, converts it into an error with the value `e`, 
   * 					otherwise, adds the error to the list of errors
   */
  def addError[E1 >: E](e:E1): Validated[A,R,E1] = {
    Validated(value.fold(
        _ => Bad(Every(e)),
        es => {
          val newEvery : Every[E1] = es :+ e 
          Bad(es :+ e)
        }
        ))
  }
  
  /**
   * Adds a list of errors to a Checker
   * @param es list of errors to add
   * @return a checker with the list of errors es
   */
  def addErrors[E1 >: E](es: Seq[E1]): Validated[A,R,E1] = {
    val zero : Validated[A,R,E1] = this
    es.foldRight(zero)((e,rest) => rest.addError(e))
  }
  
  /**
   * Returns the list of errors of this checker
   * <p> If the checker is ok, the list will be empty
   */
  def errors: Seq[E] = {
    this.fold(x => Seq(), es => es.seq)
  }
  
  /**
   * Combine two validated values when their values are ok
   * It validates if at least one of the alternatives is validated
   */
  def combineSome[A1 >: A, R1 >: R, E1 >: E](other: Validated[A1,R1,E1]): Validated[A1,R1,E1] = {
    fold(rs1 => 
      other.fold(
          rs2 => oks(rs1 ++ rs2), 
          _ => oks(rs1)), 
      _ => other
    )
  }

  /**
   * Combine two validated values when their values are ok
   * It validates if both of the alternatives are validated
   */
  def combineAll[A1 >: A, R1 >: R, E1 >: E](other: Validated[A1,R1,E1])
      : Validated[A1,R1,E1] = {
    fold(rs => 
      other.fold(
          os => oks(rs ++ os), 
          es => errs(es)), 
        es => 
          errs(es.toSeq)
    )
  }
  
  /**
   * Combine two validated values when one one of the values is ok
   * It validates if only one of the alternatives is validated
   */
  def combineOne[A1 >: A, R1 >: R, E1 >: E](other: Validated[A1,R1,E1]): Validated[A1,R1,E1] = {
    fold(rs1 => 
      other.fold(
        rs2 => 
          err(OneOfWithSeveralValid(rs1 ++ rs2)), 
        _ => 
          oks(rs1)), 
      es1 => 
        other.fold(
            rs => oks(rs),
            es2 => errs(es1 ++ es2)))
  }
  
  def reasons[A1 >: A, R1 >: R]: Option[Responses[A1,R1]] = {
    if (isOK) {
      Some(value.get)
    } else
      None
  }

}

object Validated {
  
  /**
   * Make a validated value initialized with an error
   * @tparam E type of errors
   * @param e error
   */
  def err[E >: Throwable](e: E): Validated[Nothing,Nothing,E] = 
    Validated(Bad(Every(e)))

  /**
   * Make a validated value initialized with an String msg error
   * @param msg error message
   */
  def errString(msg: String): Validated[Nothing,Nothing,Throwable] = 
    Validated(Bad(Every(MsgError(msg))))
    
  /**
   * Make a Validated value initialized with a sequence of errors
   * @tparam E type of errors
   * @param es sequence of errors
   */
  def errs[E >: Throwable](es: Seq[E]): Validated[Nothing,Nothing,E] = {
    if (es.isEmpty) throw new Exception("errors must not be empty")
    else Validated(Bad(Every.from(es).get))
  }

  /**
   * Make a validated value with an ok value
   * @tparam A type of values
   * @tparam R type of reasons
   * @param x value
   * @param reason Reason that explains why the value is valid
   */
  def ok[A,R](x: A, reason: R): Validated[A,R,Throwable] = 
    Validated(Good(Responses.single(x,reason)))

  /**
   * Make a validated value from a sequence of values/reasons
   * @tparam A type of values
   * @tparam R type of reasons
   * @param rs sequence of values/reasons
   */
  def oks[A,R](rs: Responses[A,R]): Validated[A,R,Throwable] = 
    Validated(Good(rs))

  /**
   * Make a validated value empty
   * @tparam A type of values
   * @tparam R type of reasons
   */
  def okZero[A,R](): Validated[A,R,Throwable] = 
    Validated(Good(Responses.empty))
  
  def all[A,R:Monoid,E >: Throwable](vs: Seq[Validated[A,R,E]]): Validated[Seq[A],R,E] = {
    val zero: Validated[Seq[A],R,E] = okZero()
    def next(v: Validated[A,R,E], 
        rest: Validated[Seq[A],R,E]): Validated[Seq[A],R,E] = {
      v.fold(rs => 
        rest.fold(rss => ???, //rs.combine(rss,...), // Responses(), 
            es => ???), 
          es => ???)
    }
    vs.foldRight(zero)(next)
  }
}


