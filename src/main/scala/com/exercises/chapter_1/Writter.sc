import cats.data.Writer
import cats.syntax.writer._
import cats.instances.vector._
import cats.syntax.applicative._

def slowly[A](body: => A) =
  try body finally Thread.sleep(100)

def factorial(n: Int): Int = {
  val ans = slowly(if(n == 0) 1 else n * factorial(n - 1))
  println(s"fact $n $ans")
  ans
}

type Logged[T] = Writer[Vector[String], T]
42.pure[Logged]

def factorial2(v: Int): Logged[Int] = for {
  ans <- if (v == 0) {
    1.pure[Logged]
  } else {
    slowly(factorial2(v - 1).map(_ * v))
  }
  _ <- Vector(s"fact $v $ans").tell
} yield ans