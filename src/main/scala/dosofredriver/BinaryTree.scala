package dosofredriver

object BinaryTree {

  sealed abstract class BinaryTree[+T]

  case class Node[T](value: T, left: BinaryTree[T], right: BinaryTree[T]) extends BinaryTree[T]

  case object Empty extends BinaryTree[Nothing]


  def node[T](value: T, left: BinaryTree[T], right: BinaryTree[T]): Node[T] = {
    Node(value = value, left = left, right = right)
  }

  def leaf[T](value: T): Node[T] = {
    Node[T](value = value, Empty, Empty)
  }

  def foreach[T](tree: BinaryTree[T])(f: T => Unit): Unit = {
    tree match {
      case Node(value, left, right) =>
        foreach(left)(f)
        f(value)
        foreach(right)(f)
      case Empty => ()
    }
  }
}
