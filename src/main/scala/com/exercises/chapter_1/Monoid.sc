import cats.Monoid
import cats.syntax.semigroup._
import cats.instances.int._
import cats.instances.double._
import cats.instances.option._
import cats.syntax.option._

def add(list: List[Int]): Int = {
  list.foldLeft(0)(_ + _)
}

def addM[A](list: List[A])(implicit m: Monoid[A]): A = {
  list.foldLeft(m.empty)(_ |+| _)
}

val list1 = List(1, 2, 3 ,4 ,5)
val listOpt = list1.map(_.some) ++ List(10.some, none, 11.some, none)

add(list1)

addM(list1)
addM(listOpt)

case class Order(totalCost: Double, quantity: Double)

implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
  override def empty = Ordered(Monoid[Double].empty, Monoid[Double].empty)

  override def combine(x: Order, y: Order) = Order(
    totalCost = x.totalCost |+| y.totalCost,
    quantity = x.quantity |+| y.quantity
  )
}