package dosofredriver.zipper

import dosofredriver.BinaryTree._


class Zipper[T](tree: Node[T], context: Context[T]) {

  def left: Option[Zipper[T]] = tree match {
    case Node(value, left, right) =>
      Zipper.from(left, LeftContext(value, context, right))
  }

  def right: Option[Zipper[T]] = tree match {
    case Node(value, left, right) =>
      Zipper.from(right, RightContext(value, left, context))
  }

  def up: Option[Zipper[T]] = {
    context match {
      case Top =>
        None
      case LeftContext(value, con, right) =>
        Some(Zipper(node(value, tree, right), con))
      case RightContext(value, left, con) =>
        Some(Zipper(node(value, left, tree), con))
    }
  }

  def updateValue(newValue: T): Zipper[T] = tree match {
    case Node(_, left, right) =>
      Zipper(node(newValue, left, right), context)
  }

  def update(newNode: Node[T]): Zipper[T] = {
    Zipper(newNode, context)
  }

  def erase: Option[Zipper[T]] = {
    context match {
      case Top =>
        None
      case LeftContext(value, context, bTree) =>
        Some(Zipper(node(value, Empty, bTree), context))
      case RightContext(value, bTree, context) =>
        Some(Zipper(node(value, bTree, Empty), context))
    }
  }

  def getValue: BinaryTree[T] = tree
}

object Zipper {
  def apply[T](tree: Node[T], context: Context[T]): Zipper[T] =
    new Zipper(tree, context)

  def from[T](bTree: BinaryTree[T]): Option[Zipper[T]] = {
    from(bTree, Top)
  }

  private def from[T](bTree: BinaryTree[T], context: Context[T]): Option[Zipper[T]] = {
    bTree match {
      case node: Node[T] =>
        Some(new Zipper[T](node, context))
      case Empty => None
    }
  }
}
