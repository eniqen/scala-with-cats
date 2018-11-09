import cats.Monad
import cats.data.Reader
import cats.syntax.applicative._

type DbReader[A] = Reader[Db, A]

case class Db(usernames: Map[Int, String],
              passwords: Map[String, String])

def findUsername(userId: Int): DbReader[Option[String]] = Reader(db => db.usernames.get(userId))

def checkPassword(username: String,
                  password: String): DbReader[Boolean] = Reader(db => db.passwords.get(username).contains(password))

def checkLogin(userId: Int,
               password: String): DbReader[Boolean] = for {
  userName <- findUsername(userId)
  result   <- userName.map(checkPassword(_, password)).getOrElse(false.pure[DbReader])
} yield result

val users = Map(
  1 -> "dade",
  2 -> "kate",
  3 -> "margo"
)
val passwords = Map(
  "dade"  -> "zerocool",
  "kate"  -> "acidburn",
  "margo" -> "secret"
)

val db = Db(users, passwords)
checkLogin(1, "zerocool").run(db)
checkLogin(4, "davinci").run(db)


import cats.data.State

type CalcState[A] = State[List[Int], A]

def evalOne(symbol: String): CalcState[Int] = symbol match {
  case "+" => operation(_ + _)
  case "-" => operation(_ - _)
  case "*" => operation(_ * _)
  case "/" => operation(_ / _)
  case num => operand(num.toInt)
}

def evalAll(input: List[String]): CalcState[Int] = input.foldLeft(0.pure[CalcState]){
  (acc, v) => acc.flatMap(_ => evalOne(v))
}

def operation(f:(Int, Int) => Int): CalcState[Int] = State[List[Int], Int] {
  case a :: b :: tail =>
    val ans = f(a, b)
    (ans :: tail, ans)
  case _ => sys.error("Fail!")
}

def operand(num: Int): CalcState[Int] = State[List[Int], Int](stack => (num :: stack, num))

def evalInput(input: String): Int = evalAll(input.split(" ").toList).runA(Nil).value

val program = for {
  _   <- evalOne("1")
  _   <- evalOne("2")
  ans <- evalOne("+")
} yield ans

program.runA(Nil).value

val program2 = for {
  _   <- evalAll(List("1", "2", "+"))
  _   <- evalAll(List("3", "4", "+"))
  ans <- evalOne("*")
} yield ans

program2.runA(Nil).value




sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(v) => f(v)
      case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = f(a) match {
      case Leaf(Left(v)) => tailRecM(v)(f)
      case Leaf(Right(value)) => pure(value)
      case Branch(l, r) => Branch(
        flatMap(l) {
          case Left(ll) => tailRecM(ll)(f)
          case Right(rr) => pure(rr)
        },
        flatMap(r) {
          case Left(rl) => tailRecM(rl)(f)
          case Right(rr) => pure(rr)
        }
      )
    }

    override def pure[A](x: A): Tree[A] = leaf(x)
  }

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)
}
