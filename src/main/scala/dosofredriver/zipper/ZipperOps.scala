package dosofredriver.zipper

import dosofredriver.BinaryTree.BinaryTree

object ZipperOps {
  implicit class ZipperOps[T](bTree: BinaryTree[T]) {
    def toZipper: Option[Zipper[T]] = Zipper.from(bTree)
  }
}
