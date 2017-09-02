package dosofredriver

import org.scalatest.{Inside, Matchers, WordSpec}
import BinaryTree._
import dosofredriver.zipper.Zipper
import dosofredriver.zipper.ZipperOps._

class ZipperOps extends WordSpec with Matchers with Inside {
  val left =
    node("2",
      leaf("3"),
      leaf("4"))

  val right =
    leaf("5")

  val tree =
    node("1",
      left,
      right)

  val treeZipper = tree.toZipper.get


  "A Zipper" should {

    "return left subtree" in {

      unpack(treeZipper.left) shouldEqual left
    }

    "return right subtree" in {
      unpack(treeZipper.right) shouldEqual right
    }

    "return original tree" in {
      unpack(leftUp(treeZipper)) shouldEqual tree
    }

    "update value of the left subtree" in {
      val newStr = "hello!"
      val updated = updateLeft(newStr)(treeZipper)

      unpack(updated) shouldEqual left.copy(value = newStr)
    }

    "update left subtree" in {
      val newLeaf = leaf("New leaf")
      val updated = treeZipper.left.get update newLeaf

      updated.getValue shouldEqual newLeaf
    }

    "erase left subtree" in {
      val erased = eraseLeft(treeZipper)
      inside(unpack(erased)) { case Node(_, leftSubtree, _) =>
          leftSubtree shouldEqual Empty
      }
    }
  }


  private def unpack[A]: Option[Zipper[A]] => BinaryTree[A] =
    _.get.getValue


  private def eraseLeft[A]: Zipper[A] => Option[Zipper[A]] = { zipper =>
    for {
      left <- zipper.left
      erased <- left.erase
    } yield erased
  }

  private def updateLeft[T](newValue: T): Zipper[T] => Option[Zipper[T]] = { zipper =>
    for {
      z <- zipper.left
    } yield z.updateValue(newValue)
  }


  private def leftUp[A]: Zipper[A] => Option[Zipper[A]] = { zipper =>
    for {
      left <- zipper.left
      up <- left.up
    } yield up
  }
}
