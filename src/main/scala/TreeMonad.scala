import cats.Monad

import scala.annotation.tailrec
import scala.language.higherKinds

object TreeMonad {

  sealed trait Tree[+A] {
    val depth: Int
  }

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
    override val depth: Int = 1 + Math.max(left.depth, right.depth)
    override def toString: String = if(depth<=5) s"Branch($left, $right)"
                                    else s"Branch[${left.depth},${right.depth}]"
  }

  final case class Leaf[A](value: A) extends Tree[A] {
    override val depth: Int = 1
  }

  class TreeMonad extends Monad[Tree] {

    override def pure[A](x: A): Tree[A] = Leaf(x)

    final override def flatMap[A, B](fa: Tree[A])(f: (A) => Tree[B]): Tree[B] = {
      @tailrec
      def m(s: List[Tree[A]], b: Builder[B]): Tree[B] = s match {
        case Nil => b match {
          case Result(Some(tree)) => tree
          case _ => throw new IllegalStateException(s"expected Result(Some(tree)) but was $b")
        }
        case Leaf(x) :: xs => m(xs, b(f(x)))
        case Branch(l, r) :: xs => m(l :: r :: xs, ComposeBranch(b))
      }
      m(List(fa), Result())
    }

    final override def tailRecM[A, B](a: A)(f: (A) => Tree[Either[A, B]]): Tree[B] = {
      @tailrec
      def m(s: List[Tree[Either[A, B]]], b: Builder[B]): Tree[B] = s match {
        case Nil => b match {
          case Result(Some(tree)) => tree
          case _ => throw new IllegalStateException(s"expected Result(Some(tree)) but was $b")
        }
        case Leaf(Right(x)) :: xs => m(xs, b(Leaf(x)))
        case Leaf(Left(y)) :: xs => m(f(y) :: xs, b)
        case Branch(l, r) :: xs => m(l :: r :: xs, ComposeBranch(b))
      }
      m(List(f(a)), Result())
    }

    private sealed trait Builder[A] extends Function[Tree[A], Builder[A]]

    private case class Result[A](var treeOpt: Option[Tree[A]] = None) extends Builder[A] {
      final def apply(tree: Tree[A]): Builder[A] = { treeOpt = Some(tree); this }
    }

    private case class RightLeg[A](left: Tree[A], b: Builder[A]) extends Builder[A] {
        final def apply(right: Tree[A]): Builder[A] = b(Branch(left, right))
    }

    private case class ComposeBranch[A](b: Builder[A]) extends Builder[A] {
      final def apply(left: Tree[A]): Builder[A] = RightLeg(left,b)
    }

  }

}

