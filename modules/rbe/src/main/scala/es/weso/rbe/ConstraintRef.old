package es.weso.rbe

/**
 * @param value reference value of this constraint 
 */
case class ConstraintRef(value: Int) 
   extends AnyVal
   with Ordered[ConstraintRef] {

  def compare (that: ConstraintRef) = this.value.compare(that.value)

  override def toString = s"!$value"
}
