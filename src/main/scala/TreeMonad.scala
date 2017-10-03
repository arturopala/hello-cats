import cats.Monad

import scala.annotation.tailrec
import scala.language.higherKinds

object TreeMonad {

  sealed trait Tree[+A]

  final case class Branch[A](left:Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  class TreeMonad[A] extends Monad[Tree] {

    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: (A) => Tree[B]): Tree[B] = fa match {
      case Leaf(x) => f(x)
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
    }

    @tailrec
    final override def tailRecM[A, B](a: A)(f: (A) => Tree[Either[A, B]]): Tree[B] = {

      def m(tree: => Tree[Either[A, B]]): Tree[B] = tree match {
        case Leaf(Right(r)) => Leaf(r)
        case Branch(l,r) => Branch(m(l),m(r))
        case Leaf(Left(l)) => m(f(l))
      }

      f(a) match {
        case Leaf(Right(r)) => Leaf(r)
        case Leaf(Left(l)) => tailRecM(l)(f)
        case Branch(l,r) => Branch(m(l),m(r))
      }
    }
  }

}

