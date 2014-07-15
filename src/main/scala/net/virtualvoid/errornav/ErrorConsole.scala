package net.virtualvoid.errornav

import sbt._
import xsbti.{ Position, Problem }

object ErrorConsole {
  import Repl._

  def init = observeErrors(byFile)

  val mainMenu: Parser = {
    case 'f' ⇒ observeErrors(byFile)
    case 't' => observeErrors(byErrorType)
    case 's' => observeErrors(bySubject)
    case 'm' => observeErrors(byMessage)
    case 'a' ⇒ allErrors
  }

  val byFile = by(_.position.sourcePath.getOrElse("<Unknown>"))("file", identity) _
  val byErrorType = by(p => CompilationError.parse(p.message()).productPrefix)("error type", identity) _
  val bySubject = byMany(p => CompilationError.parse(p.message()).subjects)("error subject", identity) _
  val byMessage = by(_.message)("error message", identity) _

  def observeErrors(f: Seq[Problem] => Action) = Observe(s => f(s.errors))

  def by[T](key: Problem => T)(name: String, print: T => String)(errors: Seq[Problem]) = byMany(k => Seq(key(k)))(name, print)(errors)
  def byMany[T](key: Problem => Seq[T])(name: String, print: T => String)(errors: Seq[Problem]) = {
      val grouped =
        errors.flatMap(key).groupBy(identity).mapValues(_.size).toSeq.sortBy(-_._2)
      def fileLine(l: ((T, Int), Option[Char])): String = l match {
        case ((t, num), key) => f"${key.map(_ + ")").getOrElse("  ")}%s $num%4d ${print(t)}%s\n"
      }
      def charFor(idx: Int): Option[Char] = Some(idx).filter(_ < 10).map(i => ('0' + i).toChar)
      val indexed = grouped.zipWithIndex.map(x => (x._1, charFor(x._2)))

      def screen(fileLine: (((T, Int), Option[Char])) => String): String =
          s"""${errors.size} problems (by $name)
             |
             |${indexed.map(fileLine).mkString}
             |""".stripMargin

      def standardMode =
        SetScreen(screen(fileLine)) ~
        SetParser(Parser {
          case Digit(i) if i < indexed.size =>
            val k = grouped(i)._1
            Observe(s => errorList(s.errors.filter(e => key(e).contains(k)), print(k)))
          case '/' => searchMode("")
        } orElse mainMenu)

      def searchMode(keys: String): Action =
        Observe { s =>
          def active(t: T) = keys.nonEmpty && print(t).contains(keys)
          def highlightedLine(l: ((T, Int), Option[Char])): String = {
            val line = fileLine(l)
            import scala.Console._
            if (active(l._1._1)) s"$REVERSED$line$RESET"
            else line
          }

          SetScreen(screen(highlightedLine) + s"\n\n/$keys") ~
          SetParser {
            case 0x7f if keys.isEmpty => standardMode
            case 0x7f => searchMode(keys.dropRight(1))
            case c if c >= 32 && c < 127 => searchMode(keys + c.toChar)
            case 13 =>
              def cond(p: Problem): Boolean = key(p).exists(active)
              Observe(s => errorList(s.errors.filter(cond), s"$name containing '$keys'"))
            case x =>
              println(f"Unknown char: $x%d")
              Action.noAction
          }
        }

      standardMode
    }

  val allErrors = Observe(s => errorList(s.errors, "all"))

  def errorList(errors: Seq[Problem], name: String) =
    SetScreen {
      def problemLine(p: (Problem, Int)): String = p match {
        case (p, idx) ⇒ s"${idx}) ${print(p.position, p.message)}\n"
      }

      s"""${errors.size} problems ($name)
         |
         |${errors.take(10).zipWithIndex.map(problemLine).mkString}
         |""".stripMargin
    } ~
      SetParser(Parser {
        case Digit(d) if d < errors.size ⇒
          import sys._
          val problem = errors(d)
          val cmdLine = s"/home/johannes/bin/run-idea.sh --line ${problem.position.line.get} ${problem.position.sourcePath().get}"
          //println(cmdLine)
          cmdLine.!
          Action.noAction
      } orElse mainMenu)

  object Digit {
    def unapply(i: Int): Option[Int] =
      if (i >= '0' && i <= '9') Some(i - '0')
    else None
  }

  def print(pos: Position, msg: String): String = {
    import Logger._

    if (pos.sourcePath.isEmpty && pos.line.isEmpty) msg
    else {
      val sourcePrefix = m2o(pos.sourcePath).getOrElse("")
      val lineNumberString = m2o(pos.line).map(":" + _ + ":").getOrElse(":") + " "
      val msgLine = sourcePrefix + lineNumberString + msg

      val lineContent = pos.lineContent

      val contentLines: Seq[String] =
        if (!lineContent.isEmpty) {
          val content = lineContent
          val pointer = m2o(pos.pointerSpace).map(_ + "^")
          content +: pointer.toSeq
        } else Nil

      (msgLine +: contentLines).mkString("\n")
    }
  }
}