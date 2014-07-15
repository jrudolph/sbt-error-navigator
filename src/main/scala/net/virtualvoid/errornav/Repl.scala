package net.virtualvoid.errornav

import sbt.Access

import jline.console.ConsoleReader
import xsbti.{ Severity, Problem }

import scala.annotation.tailrec

object Repl {
  case object ExitException extends RuntimeException

  type Parser = PartialFunction[Int, Action]
  def Parser(p: Parser): Parser = p
  type Screen = ReplState ⇒ String

  case class ReplState(errors: Seq[Problem], warnings: Seq[Problem])

  sealed trait Action {
    def ~(next: Action) = MultipleActions(Seq(this, next))
  }
  object Action {
    val noAction = MultipleActions(Nil)
  }
  case class MultipleActions(actions: Seq[Action]) extends Action {
    override def ~(next: Action): MultipleActions = MultipleActions(actions :+ next)
  }
  case class SetParser(parser: Parser) extends Action
  case class SetStateParser(parser: ReplState ⇒ Parser) extends Action
  case class SetScreen(screen: Screen) extends Action
  case class Alert(text: String) extends Action
  case object Quit extends Action

  trait Watcher {
    def poll(): Option[Seq[Problem]]
    def terminate(): Unit
  }

  def repl(initialize: Action, watcher: Watcher) = try Access.withJLine {
    println("Repl started!")
    val reader = new ConsoleReader()

    var state = ReplState(Nil, Nil)
    var parser: ReplState ⇒ Parser = null
    var screen: Screen = null

    runAction(initialize)

    def redraw() = {
      import ANSI._

      scala.Console.out.print(TOPLEFT + CLEAR + screen(state))
    }

    def runAction(action: Action): Boolean = action match {
      case MultipleActions(as) ⇒ as map runAction exists identity
      case SetParser(p) ⇒
        println(s"Parser is now ${p.getClass.getSimpleName}")
        parser = _ ⇒ p; false
      case SetStateParser(p) ⇒
        println(s"Parser is now ${p.getClass.getSimpleName}")
        parser = p; false
      case SetScreen(s) ⇒
        screen = s; true
      case Quit ⇒
        watcher.terminate()
        throw ExitException
        false
    }

    @tailrec def loop(): Unit = {
      var shouldRedraw = false

      watcher.poll() match {
        case Some(newProblems) ⇒
          val errors = newProblems.filter(_.severity == Severity.Error)
          val warnings = newProblems.filter(_.severity == Severity.Warn)

          state = ReplState(errors, warnings)
          shouldRedraw = true
        case None ⇒ // nothing new to see
      }

      val curParser = parser(state)
      while (System.in.available > 0) {
        val key = reader.readCharacter()
        if (key == 'q') runAction(Quit)
        else if (curParser.isDefinedAt(key)) shouldRedraw |= runAction(curParser(key))
        else println(s"Key not allowed here: '$key'")
      }

      if (shouldRedraw) redraw()

      Thread.sleep(1)

      loop()
    }

    redraw()
    loop()
  } catch {
    case ExitException ⇒
  }
}