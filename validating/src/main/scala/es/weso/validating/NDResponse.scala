package es.weso.validating


/**
 * Represents a non-deterministic response which can contain several responses
 * @tparam A Type of values
 * @tparam B Type of result values 
 * @param values sequence of deterministic responses
 */
case class NDResponse[A, B] private (
    values: Seq[Response[A, B]]) {

  /**
   * Extend the possible values with the values of another non-deterministic response
   * @param other the NDResponse whose values will be extended
   */
  def ++(other: NDResponse[A, B]): NDResponse[A, B] = {
    NDResponse(values  ++ other.values)
  }

  /**
   * Apply a function 
   * @tparam B type of values
   * @param f function to apply
   */
  def map2[C,D](f: A => C, g: B => D): NDResponse[C, D] = {
    NDResponse(values.map(r => r.map2(f,g)))
  }

  /**
   * Combine with another NDResponse applying a function to each value
   * @param other other NDResponse
   * @param f combination function
   */
  def combineWith(
    other: NDResponse[A, B],
    f: (Response[A, B], Response[A, B]) => Response[A, B]): NDResponse[A, B] = {
    if (values.isEmpty) other
    else if (other.values.isEmpty) this
    else {
      val rs = for {
        v1 <- values;
        v2 <- other.values
      } yield f(v1, v2)
      NDResponse(rs)
    }
  }

  /**
   * Apply a function to each of the possible responses
   * @param f function to apply
   */
  def mapResponse(f: Response[A, B] => Response[A, B]): NDResponse[A, B] = {
    NDResponse(values.map(f))
  }

  override def toString: String = {
    s"NDResponse[${values.toString}]"
  }
}

object NDResponse {

  /**
   * Create a single NDResponse from a Response
   */
  def single[A, B](r: Response[A,B]): NDResponse[A, B] = {
    NDResponse(Seq(r))
  }
  
  /**
   * Given a sequence of NDResponse's create a NDResponse concatenating their values 
   */
  def flatten[A,B](rss: Seq[NDResponse[A,B]]): NDResponse[A,B] = {
    def next(x: NDResponse[A,B], r: NDResponse[A,B]): NDResponse[A,B] = x ++ r
    rss.reduceLeft(next)
  }
}
