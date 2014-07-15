package net.virtualvoid.errornav

import sbt._
import xsbti.{ Position, Problem }

object ErrorConsole {
  import Repl._

  val mainMenu: Parser = {
    case 'f' ⇒ byFile
    case 't' => byErrorType
    case 's' => bySubject
    case 'm' => byMessage
    case 'a' ⇒ allErrors
  }

  val byFile = by(_.position.sourcePath.getOrElse("<Unknown>"))(identity)
  val byErrorType = by(p => CompilationError.parse(p.message()).productPrefix)(identity)
  val bySubject = byMany(p => CompilationError.parse(p.message()).subjects)(identity)
  val byMessage = by(_.message)(identity)

  def by[T](key: Problem => T)(print: T => String) = byMany(k => Seq(key(k)))(print)
  def byMany[T](key: Problem => Seq[T])(print: T => String) =
    Observe { s ⇒
      val grouped =
        s.errors.flatMap(key).groupBy(identity).mapValues(_.size).toSeq.sortBy(-_._2)
      def fileLine(l: ((T, Int), Option[Char])): String = l match {
        case ((t, num), key) => f"${key.map(_ + ")").getOrElse("  ")}%s $num%4d ${print(t)}%s\n"
      }
      def charFor(idx: Int): Option[Char] = Some(idx).filter(_ < 10).map(i => ('0' + i).toChar)
      val indexed = grouped.zipWithIndex.map(x => (x._1, charFor(x._2)))

      SetScreen(
        s"""${s.errors.size} problems
           |
           |${indexed.map(fileLine).mkString}
           |""".stripMargin) ~
      SetParser(Parser {
        case Digit(i) if i <= indexed.size =>
          val k = grouped(i)._1
          Observe(s => errorList(s.errors.filter(e => key(e).contains(k)), print(k)))
      } orElse mainMenu)
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
        case Digit(d) if d <= errors.size ⇒
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