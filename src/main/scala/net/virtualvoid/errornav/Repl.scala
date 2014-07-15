package net.virtualvoid.errornav

import sbt.Access

import jline.console.ConsoleReader
import xsbti.{ Severity, Problem }

import scala.annotation.tailrec

object Repl {
  case object ExitException extends RuntimeException

  type Parser = PartialFunction[Int, Action]
  def Parser(p: Parser): Parser = p

  case class ReplState(errors: Seq[Problem], warnings: Seq[Problem])

  sealed trait Action {
    def ~(next: Action) = MultipleActions(Seq(this, next))
  }
  object Action {
    val noAction = MultipleActions(Nil)

    def apply(actions: Seq[Action]): Action =
      actions.size match {
        case 0 => noAction
        case 1 => actions.head
        case _ => MultipleActions(actions)
      }
  }
  case class MultipleActions(actions: Seq[Action]) extends Action {
    override def ~(next: Action): MultipleActions = MultipleActions(actions :+ next)
  }
  case class SetParser(parser: Parser) extends Action
  case class Observe(state: ReplState => Action) extends Action
  case class SetScreen(screen: String) extends Action
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
    var parser: Parser = null
    var screen = ""
    case class ObservationEntry(f: ReplState => Action, lastAction: Action)
    var observers: Seq[ObservationEntry] = Nil

    runAction(initialize)

    def redraw() = {
      import ANSI._

      scala.Console.out.print(TOPLEFT + CLEAR + screen)
    }

    def runAction(action: Action): Boolean = action match {
      case MultipleActions(as) ⇒ as map runAction exists identity
      case SetParser(p) ⇒
        println(s"Parser is now ${p.getClass.getSimpleName}")
        parser = p; false
      case Observe(f) =>
        val result = f(state)

        observers :+= ObservationEntry(f, result)

        runAction(result)

      case SetScreen(s) ⇒
        screen = s; true
      case Quit ⇒
        watcher.terminate()
        throw ExitException
        false
    }

    def onStateChanged(newState: ReplState): Unit = {
      def checkCurrent(action: Action): Action = action match {
        case MultipleActions(as) =>
          Action(as.map(checkCurrent).filterNot(_ == Action.noAction))
        case SetParser(p) => if (parser == p) action else Action.noAction
        case SetScreen(s) => if (screen == s) action else Action.noAction
      }

      def update(entry: ObservationEntry): Option[ObservationEntry] = {
        val check = checkCurrent(entry.lastAction)
        if (check == Action.noAction) None
        else {
          val result = entry.f(newState)
          Some(ObservationEntry(entry.f, result))
        }
      }

      observers = observers.flatMap(update)
      observers.foreach(e => runAction(e.lastAction))
    }

    @tailrec def loop(): Unit = {
      var shouldRedraw = false

      watcher.poll() match {
        case Some(newProblems) ⇒
          val errors = newProblems.filter(_.severity == Severity.Error)
          val warnings = newProblems.filter(_.severity == Severity.Warn)

          state = ReplState(errors, warnings)
          onStateChanged(state)
          shouldRedraw = true
        case None ⇒ // nothing new to see
      }

      while (System.in.available > 0) {
        val key = reader.readCharacter()
        if (key == 'q') runAction(Quit)
        else if (parser.isDefinedAt(key)) shouldRedraw |= runAction(parser(key))
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