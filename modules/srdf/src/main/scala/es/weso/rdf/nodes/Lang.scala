package es.weso.rdf.nodes

case class Lang(lang: String) {

  // This should be the right regular expression for lang.
  // We don't use this expression because the specification does not also.
  val langtag_ex: String = "(\\A[xX]([\\x2d]\\p{Alnum}{1,8})*\\z)" +
    "|(((\\A\\p{Alpha}{2,8}(?=\\x2d|\\z)){1}" +
    "(([\\x2d]\\p{Alpha}{3})(?=\\x2d|\\z)){0,3}" +
    "([\\x2d]\\p{Alpha}{4}(?=\\x2d|\\z))?" +
    "([\\x2d](\\p{Alpha}{2}|\\d{3})(?=\\x2d|\\z))?" +
    "([\\x2d](\\d\\p{Alnum}{3}|\\p{Alnum}{5,8})(?=\\x2d|\\z))*)" +
    "(([\\x2d]([a-wyzA-WYZ](?=\\x2d))([\\x2d](\\p{Alnum}{2,8})+)*))*" +
    "([\\x2d][xX]([\\x2d]\\p{Alnum}{1,8})*)?)\\z"

  // TODO. Specification defines other ways to match languages
  def matchLanguage(other: Lang) =
    this.lang.toLowerCase == other.lang.toLowerCase

  /** Match main language. If this="en" and other="en-ca" it returns true */
  def matchMainLanguage(other: Lang) = {
    other.lang.toLowerCase.startsWith(this.lang.toLowerCase)
  }


  override def toString = lang match {
    case "" => ""
    case ls => "@" + ls
  }

  // The following code has been inspired by:
  // http://stackoverflow.com/questions/7681183/how-can-i-define-a-custom-equality-operation-that-will-be-used-by-immutable-set
  override def equals(o: Any) = o match {
    case that: Lang => that.lang.toLowerCase == this.lang.toLowerCase
    case _ => false
  }

  override def hashCode = lang.toLowerCase.hashCode
}
