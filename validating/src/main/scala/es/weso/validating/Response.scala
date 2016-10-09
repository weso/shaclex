package es.weso.validating

/**
 * Defines a validation response
 * @tparam A type of values that have been validated
 * @tparam R container of the response (it must be an instance of applicative)
 * @param response response value
 */
case class Response[A,B](value: A, response: B) {

  def map2[C,D](f: A => C, g: B => D): Response[C,D] = {
    Response(
      value = f(value),
      response = g(response)
    )
  }  
  /**
   * Applies a function to the value
   * @tparam C new type of response value
   * @param f function that is applied
   */
  def mapValue[C](f: A => C): Response[C, B] = {
    this.copy(value = f(value))
  }

  
  /**
   * Applies a function to the value
   * @tparam C new type of response value
   * @param f function that is applied
   */
  def mapResponse[C](f: B => C): Response[A, C] = {
    this.copy(response = f(response))
  }

  override def toString:String = {
    s"Response[$value,$response]"
  }
}

object Response {

}
