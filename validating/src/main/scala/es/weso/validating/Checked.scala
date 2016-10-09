package es.weso.validating
import cats._
import cats.syntax.all._
import cats.instances.all._
import cats.implicits._

/**
 * Checked defines values that have been checked.
 * A checked value can contain a sequence of responses
 * or a list of errors.
 * @tparam A type of values that are validated
 * @tparam R type of responses
 * @tparam E type of errors
 */
trait Checked[A, R, E] {

 /**
   * @return `true` if the value has no errors
   */
  def isOK: Boolean

  def isNotOK: Boolean = !isOK

  /**
   *  Fold a checked value
   */
  def fold[V](oks: (Option[A], Seq[R]) => V, errs: (Option[A], Seq[E]) => V): V

  /**
   * Sequence of responses in case it is valid or empty sequence if it is not valid
   */
  def responses: Seq[R]

  def value: Option[A]

  /**
   * Sequence of errors in case it is invalid of empty sequence if it is valid
   */
  def errors: Seq[E]

  def mapValue[B](f: A => B): Checked[B,R,E]
  
  def mapErrors[E1](f: E => E1): Checked[A, R, E1]

  def addError(e: E): Checked[A, R, E] =
    addErrors(Seq(e))

  def addErrors(es: Seq[E]): Checked[A, R, E]
    
}


abstract class CheckedOps {
  
  def err[A,R,E](e:E): Checked[A,R,E] = err(None,e)

  /**
   * Make a checked value initialized with an error
   * @tparam A type of values
   * @tparam R type of responses
   * @tparam E type of errors
   * @param v value
   * @param e error
   */
   def err[A,R,E](x:Option[A],e:E): Checked[A,R,E]

     /**
   * Make a Checked value initialized with a sequence of errors
   * @tparam E type of errors
   * @param es sequence of errors
   */
  def errs[A, R, E](v: Option[A], es: Seq[E]): Checked[A, R, E]

 def okZeroSeq[A, R, E](): Checked[Seq[A], R, E]
  
 def ok[A, R, E](x: Option[A], r: R): Checked[A, R, E]
   
}

