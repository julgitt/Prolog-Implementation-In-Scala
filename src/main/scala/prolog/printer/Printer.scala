package prolog.printer

import prolog.ast.*

import scala.annotation.tailrec

object Printer {
  private def mainColor(s: String): String = s"\u001b[38;5;199m$s\u001b[0m"
  private def subColor(s: String): String = s"\u001b[38;5;212m$s\u001b[0m"
  private def sub2Color(s: String): String = s"\u001b[38;5;239m$s\u001b[0m"

  def printError(error: String): Unit =
    println(s"\u001b[1;31m$error    ඞ sus ඞ\u001b[0m")

  def printTrue(): Unit =
    println("\u001b[38;5;40mtrue.   ( • ᴗ - ) ✧\n\u001b[0m")

  def printFalse(): Unit =
    println("\u001b[38;5;196mfalse.   ( • ᴖ • ｡)\n\u001b[0m")
  
  @tailrec
  def view(t: Term): Term = t match {
    case Var(_, Some(sub)) => view(sub)
    case _ => t
  }

  def printTerm(t: Term): Unit = view(t) match {
    case Var(name, _) => print(subColor(name))
    case Sym(name, args) if args.isEmpty => print(mainColor(name))
    case Sym(name, args) =>
      print(mainColor(name))
      print("(")
      printTerms(args)
      print(")")
    case Num(value) => print(value.toString)
  }

  @tailrec
  def printTerms(terms: List[Term]): Unit = {
    terms match {
      case Nil => ()
      case head :: Nil => printTerm(head)
      case head :: tail =>
        printTerm(head)
        print(", ")
        printTerms(tail)
    }
  }

  def printClause(c: Clause): Unit = c match {
    case Fact(t) =>
      printTerms(List(t))
      println(".")
    case Rule(head, body) =>
      printTerms(List(head))
      print(" :- ")
      printTerms(body)
      println(".")
  }

  def printClauses(clauses: List[Clause]): Unit = {
    clauses.foreach(printClause)
  }

  def printResults(results: List[(Variable, Substitution)]): Unit = results match {
    case Nil => ()
    case (name, value) :: Nil =>
      print(s"${subColor(name)} = ")
      value match {
        case Some(t) => printTerms(List(t))
        case None => ()
      }
    case head :: tail =>
      printResults(List(head))
      println()
      printResults(tail)
  }
}
