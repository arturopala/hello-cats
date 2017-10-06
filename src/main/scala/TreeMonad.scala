import cats.Monad

import scala.annotation.tailrec
import scala.language.higherKinds

object TreeMonad {

  sealed trait Tree[+A] {
    val depth: Int
  }

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
    override val depth = 1 + Math.max(left.depth, right.depth)

    override def toString: String = s"Branch[${left.depth},${right.depth}]"
  }

  final case class Leaf[A](value: A) extends Tree[A] {
    override val depth: Int = 1
  }

  class TreeMonad extends Monad[Tree] {

    override def pure[A](x: A): Tree[A] = Leaf(x)

    final override def flatMap[A, B](fa: Tree[A])(f: (A) => Tree[B]): Tree[B] = fa match {
      case Leaf(x) => f(x)
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
    }

    final override def tailRecM[A, B](a: A)(f: (A) => Tree[Either[A, B]]): Tree[B] = {

      ???
    }
  }

}

