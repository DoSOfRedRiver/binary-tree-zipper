package dosofredriver.zipper

import dosofredriver.BinaryTree.BinaryTree

sealed trait Context[+E]
case object Top extends Context[Nothing]
case class LeftContext[E](value: E, context: Context[E], bTree: BinaryTree[E]) extends Context[E]
case class RightContext[E](value: E, bTree: BinaryTree[E], context: Context[E]) extends Context[E]
