package prolog.ast

// Aliases
type Symbol = String
type Variable = String
type Substitution = Option[Term]

// Terms
sealed trait Term
case class Var(name: Variable, substitution: Substitution) extends Term
case class Sym(name: Symbol, args: List[Term]) extends Term
case class Num(value: Int) extends Term

// Clauses
sealed trait Clause
case class Fact(term: Term) extends Clause
case class Rule(head: Term, body: List[Term]) extends Term

type Program = List[Clause]

// Queries
sealed trait Query
case class QueryTerms(terms: List[Term]) extends Query
case class Filepath(path: String) extends Query