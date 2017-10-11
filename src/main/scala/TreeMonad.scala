import java.util

import TreeMonad.Tree
import cats.Monad

import scala.annotation.tailrec
import scala.language.higherKinds

object TreeMonad {

  sealed trait Tree[+A] {
    val depth: Int
  }

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
    override val depth = 1 + Math.max(left.depth, right.depth)

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
      def m(stack: List[Tree[A]], c: Builder[B]): Tree[B] = stack match {
        case Nil => c match {
          case Result(Some(tree)) => tree
          case _ => throw new IllegalStateException(s"expected Result(Some(tree)) but was $c")
        }
        case Leaf(x) :: xs => m(xs, c(Some(f(x))))
        case Branch(l, r) :: xs => m(l :: r :: xs, ComposeBranch(c))
      }
      m(List(fa), Result())
    }

    final override def tailRecM[A, B](a: A)(f: (A) => Tree[Either[A, B]]): Tree[B] = {
      @tailrec
      def m(stack: List[Tree[Either[A, B]]], c: Builder[B]): Tree[B] = stack match {
        case Nil => c match {
          case Result(Some(tree)) => tree
          case _ => throw new IllegalStateException(s"expected Result(Some(tree)) but was $c")
        }
        case Leaf(Right(x)) :: xs => m(xs, c(Some(Leaf(x))))
        case Leaf(Left(y)) :: xs => m(f(y) :: xs, c)
        case Branch(l, r) :: xs => m(l :: r :: xs, ComposeBranch(c))
      }
      m(List(f(a)), Result())
    }

    sealed trait Builder[A] extends Function[Option[Tree[A]], Builder[A]]

    case class Result[A](var tree: Option[Tree[A]] = None) extends Builder[A] {
      def apply(o: Option[Tree[A]]): Builder[A] = { tree = o; this }
    }

    case class ComposeBranch[A](c: Builder[A]) extends Builder[A] {
      def apply(o1: Option[Tree[A]]): Builder[A] = o1 match {
        case None => c
        case Some(left) => new Builder[A] {
          def apply(o2: Option[Tree[A]]): Builder[A] = o2 match {
            case None => c(Some(left))
            case Some(right) => c(Some(Branch(left, right)))
          }
        }
      }
    }

  }

}

