import TreeMonad.{Branch, Leaf, Tree, TreeMonad}
import cats.Monad
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
    case Nil => t
    case _ :: xs => tree(xs, Branch(t, t))
  }

  implicit def arbitraryTree[A](implicit a: Arbitrary[A]): Arbitrary[Tree[A]] =
    Arbitrary(Gen.nonEmptyListOf(a.arbitrary).map {
      list =>
        val t = tree(list.tail, Leaf(list.head))
        println(s"tree depth = ${t.depth}")
        t
    })

  implicit def eqTree[A](implicit t: Eq[A]): Eq[Tree[A]] = new Eq[Tree[A]] {
    def eqv(x: Tree[A], y: Tree[A]): Boolean = {
      x == y
    }
  }

  implicit val monad = new TreeMonad

  test("flatMap stack safety") {
    val gen = Gen.nonEmptyListOf(Gen.const(1)).map(list => tree(list, Leaf(list.head)))
    forAll(gen) { (tree: Tree[Int]) =>
      println(s"tree depth = ${tree.depth}")
      Monad[Tree].flatMap(tree)(x => Leaf(x + 1))
      println("done")
    }
  }

  test("stack overflow") {
    val t = new TreeMonad()
    def b(a: Int): Tree[Either[Int,Int]] = {
      Branch(Leaf(Left(a)), Leaf(Right(1)))
    }
    t.tailRecM(1)(a => Branch(b(a), b(a)))
  }

  checkAll("TreeMonad", MonadTests[Tree].monad[Int, Int, Int])


}
