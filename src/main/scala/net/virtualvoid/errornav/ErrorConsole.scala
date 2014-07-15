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
    case 'l' ⇒ errorList
  }

  val byFile = by(_.position.sourcePath.getOrElse("<Unknown>"))(identity)
  val byErrorType = by(p => CompilationError.parse(p.message()).productPrefix)(identity)
  val bySubject = byMany(p => CompilationError.parse(p.message()).subjects)(identity)
  val byMessage = by(_.message)(identity)

  def by[T](key: Problem => T)(print: T => String) = byMany(k => Seq(key(k)))(print)
  def byMany[T](key: Problem => Seq[T])(print: T => String) =
    SetScreen { s ⇒
      val grouped =
        s.errors.flatMap(key).groupBy(identity).mapValues(_.size).toSeq.sortBy(-_._2)
      def fileLine(l: (T, Int)): String = f"${l._2}%4d ${print(l._1)}%s\n"

      s"""${s.errors.size} problems
         |
         |${grouped.map(fileLine).mkString}
         |""".stripMargin

    } ~ SetParser(mainMenu)

  val errorList =
    SetScreen { s ⇒
      def problemLine(p: (Problem, Int)): String = p match {
        case (p, idx) ⇒ s"${idx}) ${print(p.position, p.message)}\n"
      }

      s"""${s.errors.size} problems
         |
         |${s.errors.take(10).zipWithIndex.map(problemLine).mkString}
         |""".stripMargin
    } ~ Observe { s =>
      SetParser(Parser {
        case Digit(d) if d <= s.errors.size ⇒
          import sys._
          val problem = s.errors(d)
          val cmdLine = s"/home/johannes/bin/run-idea.sh --line ${problem.position.line.get} ${problem.position.sourcePath().get}"
          //println(cmdLine)
          cmdLine.!
          Action.noAction
      } orElse mainMenu)
    }

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