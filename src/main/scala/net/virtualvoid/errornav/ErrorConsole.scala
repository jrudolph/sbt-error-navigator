package net.virtualvoid.errornav

import sbt._
import xsbti.{ Position, Problem }

object ErrorConsole {
  import Repl._

  val mainMenu: Parser = {
    case 'O' ⇒ overview
    case 'E' ⇒ top10
  }

  val overview =
    SetScreen { s ⇒
      val byFile =
        s.errors.groupBy(_.position.sourcePath.getOrElse("<Unknown>")).mapValues(_.size).toSeq.sortBy(-_._2)
      def fileLine(l: (String, Int)): String = f"${l._2}%4d ${l._1}%s\n"

      s"""${s.errors.size} problems
         |
         |${byFile.map(fileLine).mkString}
         |""".stripMargin

    } ~ SetParser(mainMenu)

  val top10 =
    SetScreen { s ⇒
      def problemLine(p: (Problem, Int)): String = p match {
        case (p, idx) ⇒ s"${idx}) ${print(p.position, p.message)}\n"
      }

      s"""${s.errors.size} problems
         |
         |${s.errors.take(10).zipWithIndex.map(problemLine).mkString}
         |""".stripMargin
    } ~ SetStateParser(s ⇒ Parser {
      case Digit(d) if d <= s.errors.size ⇒
        import sys._
        val problem = s.errors(d)
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