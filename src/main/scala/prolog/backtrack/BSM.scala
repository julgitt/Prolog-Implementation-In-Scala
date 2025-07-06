package prolog.backtrack

import prolog.ast._

object BSM {

  type BSM[A] = Stack.Stack => (A, Stack.Stack)

  def backtrackGoals(): BSM[List[Term]] = { stack =>
    Stack.pop(stack) match {
      case (None, emptyStack) =>
        (Nil, emptyStack)
      case (Some(_), newStack) =>
        if (Stack.isEmpty(newStack)) {
          (Nil, newStack)
        } else {
          val topStateOpt = Stack.top(newStack)
          topStateOpt match {
            case Some(topState) =>
              val clearedStack = StateOps.clearTempSubstitutions(newStack)
              (topState.goals, clearedStack)
            case None =>
              (Nil, newStack)
          }
        }
    }
  }

  def initialize(program: Program, variables: List[(Variable, Substitution)]): Stack.Stack = {
    val initialState = StateOps.makeState(
      program,
      tempSubs = Nil,
      variables = variables,
      goals = Nil,
      clauses = program
    )
    Stack.initialize(initialState)
  }

  def pure[A](x: A): BSM[A] = stack => (x, stack)

  def flatMap[A, B](m: BSM[A])(f: A => BSM[B]): BSM[B] = stack => {
    val (a, stack2) = m(stack)
    f(a)(stack2)
  }

  def map[A, B](m: BSM[A])(f: A => B): BSM[B] = stack => {
    val (a, stack2) = m(stack)
    (f(a), stack2)
  }

  def run[A](computation: BSM[A]): A = {
    val (res, _) = computation(Stack.empty)
    res
  }
}
