trait Monad[F[_]] {
  def pure[A](value : A): F[A]
  def flatMap[A, B](value: F[A])(f: A => F[B]): F[B]
  def map[A, B](value: F[A])(f: A => B): F[B] = flatMap(value)(a => pure(f(a)))
}

object Id {
  type Id[A] = A

  def pure[A](value: A): Id[A] = value
  def flatMap[A, B](value: Id[A])(f: A => Id[B]): Id[B] = f(value)
  def map[A, B](value: Id[A])(f: A => B): Id[B] = f(value)
}

import cats.{Apply, Monad}
import cats.instances.option._
import cats.instances.list._

import scala.language.higherKinds
val listOpt = Apply[List] compose Apply[Option]
val plusOne = (x: Int) ⇒ x + 1
listOpt.ap(List(Some(plusOne)))(List(Some(1), None, Some(3)))

Apply[Option].tuple2(Some(1), Some(2))

import cats.implicits._

val addArity2 = (a: Int, b: Int) ⇒ a + b
val addArity3 = (a: Int, b: Int, c: Int) ⇒ a + b + c

val option2 = Option(1) |@| Option(2)
val option3 = option2 |@| Option.empty[Int]

option2 map addArity2
option3 map addArity3

option2 apWith Some(addArity2)
option3 apWith Some(addArity3)

option2.tupled
option3.tupled

Monad[Option].ifM(Option(true))(Option("truthy"), Option("falsy"))

Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4))


