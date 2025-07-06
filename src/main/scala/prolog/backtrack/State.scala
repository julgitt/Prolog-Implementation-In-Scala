package prolog.backtrack

import prolog.ast._

case class State (
  program: Program,
  tempSubstitutions: List[(Variable, Substitution)],
  variables: List[(Variable, Substitution)],
  goals: List[Term],
  clauses: List[Clause]
)