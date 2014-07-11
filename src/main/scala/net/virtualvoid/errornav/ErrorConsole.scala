package net.virtualvoid.errornav

import sbt._

object ErrorConsole {
  import Repl._

  val mainMenu: Parser = {
    case 'o' ⇒ overview
  }

  val overview =
    SetScreen { s ⇒
      val byFile =
        s.problems.groupBy(_.position.sourcePath.getOrElse("<Unknown>")).mapValues(_.size).toSeq.sortBy(-_._2)
      def fileLine(l: (String, Int)): String = f"${l._2}%4d ${l._1}%s\n"

      s"""${s.problems.size} problems
         |
         |${byFile.map(fileLine).mkString}
         |""".stripMargin

    } ~ SetParser(mainMenu)
}