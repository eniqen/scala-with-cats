import cats.{Apply, Functor, Monad}

import scala.language.higherKinds
import cats.syntax.functor._

//trait Functor[F[_]] {
//  def map[A, B](fa: F[A])(f: A => B): F[B]
//}
//
//object Functor {
//  object laws {
//    def identity[F[_], A](src: F[A])(implicit fa: Functor[F]): Boolean =
//      fa.map(src)(x => x) == src
//
//    def composition[F[_], A, B](src: F[A])(f: A => B, g: A => B)(
//        implicit fa: Functor[F]): Boolean = ???
//  }
//}

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](leaf: A): Tree[A] = Leaf(leaf)
}

implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
  override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
    case Leaf(value) => Leaf(f(value))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
}


val tree = Branch(Branch(Leaf(5), Branch(Leaf(7), Leaf(9))), Leaf(10))

val value = Functor[Tree].map(tree)(_ * 2)

Tree.leaf(200).map(_ + 3)
Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 7)

trait Printable[A] {
  self =>

  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] = (value: B) => self.format(func(value))
}

final case class Box[A](value: A)

implicit val stringPrintable: Printable[String] =
  new Printable[String] {
    def format(value: String): String =
      "\"" + value + "\""
  }

implicit val booleanPrintable: Printable[Boolean] = new Printable[Boolean] {
  def format(value: Boolean): String =
    if(value) "yes" else "no"
}

implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] = (box: Box[A]) => p.format(box.value)
implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] = p.contramap[Box[A]](_.value)

