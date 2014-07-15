package net.virtualvoid.errornav

sealed trait CompilationError extends Product {
  def subjects: Seq[String]
}
object CompilationError {
  val NotFoundP = """not found: (.*)""".r
  val NotAMemberP = """(.+) is not a member of (.+)""".r
  val ImplicitValueNotFoundP = """could not find implicit value for parameter (.*): (.*)""".r
  val ImplicitEvidenceValueNotFoundP = """could not find implicit value for evidence parameter of type (.*)""".r
  val TooManyArgumentsP = """too many arguments for method (.*): (.*)""".r
  val TypeMismatchP = """type mismatch;
                        | found   : (.*)
                        | required: (.*)""".stripMargin.r

  case class NotFound(what: String) extends CompilationError {
    def subjects: Seq[String] = Seq(what)
  }
  case class NotAMember(what: String, of: String) extends CompilationError {
    def subjects: Seq[String] = Seq(what, of)
  }
  case class ImplicitValueNotFound(parameter: String, tpe: String) extends CompilationError {
    def subjects: Seq[String] = Seq(tpe)
  }
  case class ImplicitEvidenceValueNotFound(tpe: String) extends CompilationError {
    def subjects: Seq[String] = Seq(tpe)
  }
  case object MissingParameterType extends CompilationError {
    def subjects: Seq[String] = Nil
  }
  case class TooManyArguments(method: String, signature: String) extends CompilationError {
    def subjects: Seq[String] = Seq(method)
  }
  case class TypeMismatch(found: String, required: String) extends CompilationError {
    def subjects: Seq[String] = Seq(found, required)
  }

  case class OtherError(message: String) extends CompilationError {
    def subjects: Seq[String] = Nil
  }

  def parse(message: String): CompilationError = message match {
    case NotFoundP(what) => NotFound(what)
    case NotAMemberP(what, of) => NotAMember(what, of)
    case ImplicitValueNotFoundP(p, tpe) => ImplicitValueNotFound(p, tpe)
    case ImplicitEvidenceValueNotFoundP(tpe) => ImplicitEvidenceValueNotFound(tpe)
    case "missing parameter type" => MissingParameterType
    case TooManyArgumentsP(method, sig) => TooManyArguments(method, sig)
    case TypeMismatchP(found, req) => TypeMismatch(found, req)
    case _ => OtherError(message)
  }
}

