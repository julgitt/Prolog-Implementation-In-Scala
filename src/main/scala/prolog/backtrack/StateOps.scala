package prolog.backtrack

import prolog.ast._
import prolog.printer._

object StateOps {
  import Stack._

  def makeState(
    program: Program,
    tempSubs: List[(Variable, Substitution)],
    variables: List[(Variable, Substitution)],
    goals: List[Term],
    clauses: List[Clause]
  ): State = State(program, tempSubs, variables, goals, clauses)

  def printState(state: State): Unit = {
    println("Clauses:")
    Printer.printClauses(state.clauses)
    println("\nGoals:")
    Printer.printTerms(state.goals)
    println("\nProductions:")
    Printer.printResults(state.variables)
    println("\nSubstitutions:")
    Printer.printResults(state.tempSubstitutions)
    println("\n")
  }

  def printStack(stack: Stack): Unit = stack.foreach(printState)

  def getProgram(stack: Stack): Option[Program] = top(stack).map(_.program)

  def getGoals(stack: Stack): Option[List[Term]] = top(stack).map(_.goals)

  def getClauses(stack: Stack): Option[List[Clause]] =
    top(stack).map(_.clauses)

  def getSubstitutions(stack: Stack): List[(Variable, Substitution)] =
    top(stack).map(_.variables.filter(_._2.isDefined)).getOrElse(Nil)

  def pushTempSubstitution(sub: (Variable, Option[Term]), stack: Stack): Stack = {
    top(stack) match {
      case Some(s) =>
        val newState = s.copy(tempSubstitutions = sub :: s.tempSubstitutions)
        updateTop(newState, stack)
      case None => stack
    }
  }

  def clearTempSubstitutions(stack: Stack): Stack = {
    top(stack) match {
      case Some(s) =>
        val newState = s.copy(tempSubstitutions = Nil)
        updateTop(newState, stack)
      case None => stack
    }
  }

  def findSubstitution(variable: Variable, subs: List[(Variable, Substitution)]): Option[Option[Term]] =
    subs.find(_._1 == variable).map(_._2)

  def allSubstitutions(stack: Stack): List[(Variable, Substitution)] =
    top(stack).map(s => s.variables ++ s.tempSubstitutions).getOrElse(Nil)

  def setSubstitutions(stack: Stack): Stack = {
    top(stack) match {
      case Some(s) =>
        val newVars = s.variables.map {
          case (v, None) =>
            findSubstitution(v, s.tempSubstitutions) match {
              case Some(newSub) => (v, newSub)
              case None => (v, None)
            }
          case pair => pair
        }

        val newState = s.copy(variables = newVars)
        updateTop(newState, stack)
      case None => stack
    }
  }

  def pushSubstitution(sub: (Variable, Option[Term]), stack: Stack): (Boolean, Stack) = {
    val all = allSubstitutions(stack)
    val current = findSubstitution(sub._1, all)

    (sub._2, current) match {
      case (Some(Var(name, _)), Some(Some(existing))) =>
        (true, pushTempSubstitution((name, Some(existing)), stack))

      case (_, Some(value)) if value == sub._2 =>
        (true, stack)

      case (_, Some(_)) =>
        (false, clearTempSubstitutions(stack))

      case (_, None) =>
        (true, pushTempSubstitution(sub, stack))
    }
  }

  def updateVariables(stack: Stack): List[(Variable, Option[Term])] = {
    def update(term: Term): Term = term match {
      case Var(name, None) =>
        findSubstitution(name, allSubstitutions(stack)) match {
          case Some(Some(sub)) => update(sub)
          case _ => term
        }
      case Sym(name, terms) =>
        Sym(name, terms.map(update))
      case _ => term
    }

    top(stack).map(_.variables.map {
      case (v, Some(t)) => (v, Some(update(t)))
      case pair => pair
    }).getOrElse(Nil)
  }

  def applySubstitutions(goals: List[Term], stack: Stack): List[Term] = {
    def update(term: Term): Term = term match {
      case Var(name, None) =>
        findSubstitution(name, allSubstitutions(stack)) match {
          case Some(sub) => Var(name, sub)
          case None => term
        }
      case Sym(name, ts) =>
        Sym(name, ts.map(update))
      case _ => term
    }

    goals.map(update)
  }

  def setGoals(goals: List[Term], stack: Stack): Stack = {
    top(stack) match {
      case Some(s) =>
        val newState = s.copy(goals = goals, variables = updateVariables(stack), tempSubstitutions = Nil)
        updateTop(newState, stack)
      case None => stack
    }
  }

  def setClauses(clauses: List[Clause], stack: Stack): Stack = {
    top(stack).map { s =>
      val newState = s.copy(clauses = clauses)
      updateTop(newState, stack)
    }.getOrElse(stack)
  }

  def restoreClauses(stack: Stack): Stack = {
    top(stack).map { s =>
      val newState = s.copy(clauses = s.program)
      updateTop(newState, stack)
    }.getOrElse(stack)
  }
}