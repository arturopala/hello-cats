trait Semigroup[A]{
  def combine(x:A,y:A): A
}


trait Monoid[A] extends Semigroup[A]{
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]) = monoid
}

object BooleanMonoids {

  val And = new Monoid[Boolean]{
    def empty = true
    def combine(x: Boolean, y: Boolean): Boolean = x & y
  }

  val Or = new Monoid[Boolean]{
    def empty = false
    def combine(x: Boolean, y: Boolean): Boolean = x | y
  }

  val Xor = new Monoid[Boolean]{
    def empty = false
    def combine(x: Boolean, y: Boolean): Boolean = (x & !y) | (!x & y)
  }

  val Nand = new Monoid[Boolean]{
    def empty = true
    def combine(x: Boolean, y: Boolean): Boolean = !(x & y)
  }

  val Nor = new Monoid[Boolean]{
    def empty = true
    def combine(x: Boolean, y: Boolean): Boolean = !(x | y)
  }

}

object SetMonoids {

  def union[A] = new Monoid[Set[A]]{
    def empty = Set[A]()
    def combine(x: Set[A], y: Set[A]): Set[A] = x.union(y)
  }

  def intersection[A] = new Monoid[Set[A]]{
    def empty = Set[A]()
    def combine(x: Set[A], y: Set[A]): Set[A] = x.intersect(y)
  }


}