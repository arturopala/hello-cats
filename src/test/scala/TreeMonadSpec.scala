import TreeMonad.{Branch, Leaf, Tree, TreeMonad}
import cats.kernel.Eq
import cats.laws.discipline.MonadTests
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}
import org.typelevel.discipline.scalatest.Discipline

import scala.annotation.tailrec

class TreeMonadSpec extends FunSuite with Matchers with PropertyChecks with Discipline {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(minSize = 0, maxSize = 1E5.toInt)

  import cats.instances.int._
  import cats.instances.tuple._

  @tailrec
  private def tree[A](list: List[A], t: Tree[A]): Tree[A] = list match {
    case x :: Nil => Leaf(x)
    case x :: xs => tree(xs, Branch(Leaf(x), t))
  }

  implicit def arbitraryTree[A](implicit a: Arbitrary[A]): Arbitrary[Tree[A]] =
    Arbitrary(Gen.nonEmptyListOf(a.arbitrary).map(list => tree(list, Leaf(list.head))))

  implicit def eqTree[A](implicit t: Eq[A]): Eq[Tree[A]] = new Eq[Tree[A]] {
    def eqv(x: Tree[A], y: Tree[A]): Boolean = x match {
      case Leaf(a1) => y match {
        case Leaf(a2) if a1 == a2 => true
        case _ => false
      }
      case Branch(l1, r1) => y match {
        case Branch(l2, r2) if eqv(l1, l2) && eqv(r1, r2) => true
        case _ => false
      }
    }
  }

  implicit val monad = new TreeMonad[Int]

  checkAll("TreeMonad", MonadTests[Tree].monad[Int, Int, Int])

}
