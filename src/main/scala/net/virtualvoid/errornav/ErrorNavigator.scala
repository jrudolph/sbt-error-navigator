package net.virtualvoid.errornav

import java.util.concurrent.atomic.{ AtomicBoolean, AtomicReference }

import sbt.Keys._
import sbt.{ Logger, _ }
import xsbti.{ compile ⇒ _, _ }

import scala.annotation.tailrec

object ErrorNavigator {
  /*

  run a compile
  collect errors
  start special error handling console mode
  meanwhile poll for changes

  displays:
    * error summary grouped by
      * error types
      * files
      * packages
    * single error

  */

  object MyReporter extends Reporter {
    var probs = Seq.empty[Problem]

    def has(severity: Severity) = problems.exists(_.severity == severity)

    def hasErrors: Boolean = has(Severity.Error)
    def hasWarnings: Boolean = has(Severity.Warn)
    def comment(pos: Position, msg: String): Unit = {}

    def log(pos: Position, msg: String, sev: Severity): Unit =
      probs :+= Logger.problem("", pos, msg, sev)

    def problems(): Array[Problem] = probs.toArray

    def printSummary(): Unit = {
      //println(s"Got ${probs.size} problems")
    }
    def reset(): Unit = {
      //println("Reset was called!")
      probs = Seq.empty
    }
  }

  val prefix = "!@!"
  val errorNavigator =
    Command(prefix, ("", "Runs the command continuously and analyses errors"), "")(BasicCommands.otherCommandParser) { (s, arg) ⇒
      CommandUtil.withAttribute(s, Watched.Configuration, "Continuous execution not configured.") { w ⇒
        val repeat = prefix + (if (arg.startsWith(" ")) arg else " " + arg)
        executeContinuously(w, s, arg, repeat)
      }
    }

  val terminateWatch = new AtomicBoolean(false)
  val transfer = new AtomicReference[Seq[Problem]](null)
  def setValue(problems: Seq[Problem]) = transfer.set(problems)
  @tailrec def poll(): Option[Seq[Problem]] = {
    val res = transfer.get()
    if (res == null) None
    else if (transfer.compareAndSet(res, null)) Some(res)
    else poll()
  }

  def executeContinuously(watched: Watched, s: State, next: String, repeat: String): State = {
    import sbt.BasicCommandStrings._
    import sbt.Watched._

    def shouldTerminate: Boolean = terminateWatch.get //(System.in.available > 0) && (watched.terminateWatch(System.in.read()) || shouldTerminate)

    val sourcesFinder = PathFinder { watched watchPaths s }
    val watchState = s get ContinuousState getOrElse WatchState.empty

    if (watchState.count > 0) {
      //printIfDefined(watched watchingMessage watchState)
      //println(s"Trying to transport ${MyReporter.probs.size} problems")
      setValue(MyReporter.probs)
      MyReporter.reset()
    } else {
      new Thread {
        override def run(): Unit = Repl.repl(ErrorConsole.init, new Repl.Watcher {
          def poll(): Option[Seq[Problem]] = ErrorNavigator.poll()

          def terminate(): Unit = terminateWatch.set(true)
        })
      }.start()
      terminateWatch.set(false)
    }

    val (triggered, newWatchState, newState) =
      try {
        val (triggered, newWatchState) = SourceModificationWatch.watch(sourcesFinder, watched.pollInterval, watchState)(shouldTerminate)
        val res = (triggered, newWatchState, s)
        //println(s"Result: $res")
        res
      } catch {
        case e: Exception ⇒
          val log = s.log
          log.error("Error occurred obtaining files to watch.  Terminating continuous execution...")
          MainLoop.handleException(e, s, log)
          (false, watchState, s.fail)
      }

    if (triggered) {
      //printIfDefined(watched triggeredMessage newWatchState)
      (ClearOnFailure :: next :: FailureWall :: repeat :: s).put(ContinuousState, newWatchState)
    } else {
      while (System.in.available() > 0) System.in.read()
      s.put(ContinuousState, WatchState.empty)
    }
  }

  val ErrorNav = config("errornav")
  val ErrorNavTest = config("errornavtest")

  def settings =
    Seq(commands += errorNavigator) ++ forConfig(ErrorNav, Compile, Defaults.compileSettings) ++ forConfig(ErrorNavTest, Test, Defaults.testSettings)

  def forConfig(config: Configuration, oldConfig: Configuration, defaultSettings: Seq[Setting[_]]) =
    inConfig(config)(defaultSettings ++ Seq(
      Access.compilerReporter in compile := Some(MyReporter),
      compileInputs in compile <<= compileInputs in compile in oldConfig))
}