package es.weso.shacl.report

case class SingleTestReport(
  passed: Boolean, // True if test passed
  name: String, // Name of test
  uriTest: String, // URI of test
  testType: String, // Type of test
  moreInfo: String // Info about what happened
) {

  override def toString: String =
    if (passed) testType + ". OK " + name +
      ", uri: " + uriTest + ". " + moreInfo
    else testType + ". Failed " + name +
      ", uri: " + uriTest + ". " + moreInfo

}
