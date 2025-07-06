package prolog.backtrack

object Stack {
  type Stack = List[State]

  def empty: Stack = List.empty

  def isEmpty(stack: Stack): Boolean = stack.isEmpty

  def top(stack: Stack): Option[State] = stack.headOption

  def push(state: State, stack: Stack): Stack = state :: stack

  def pop(stack: Stack): (Option[State], Stack) = (stack.headOption, stack.drop(1))

  def updateTop(state: State, stack: Stack): Stack = state :: stack.drop(1)

  def initialize(state: State): Stack = List(state)
}
