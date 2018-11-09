import cats.data.State
import cats.syntax.applicative._

type CalcState[A] = State[List[Int], A]

def evalOne(symbol: String): CalcState[Int] = symbol match {
  case "+" => operator(_ + _)
  case "-" => operator(_ - _)
  case "*" => operator(_ * _)
  case "/" => operator(_ / _)
  case num => operand(num.toInt)
}

def operand(v: Int) : CalcState[Int] = State[List[Int], Int] {
  stack => (v :: stack, v)
}

def operator(f: (Int, Int) => Int) : CalcState[Int] = State[List[Int], Int] {
  case a :: b :: tail =>
    val ans = f(a, b)
    (ans :: tail, ans)
  case _ => sys.error("Fail!")
}

def evalAll(input: List[String]): CalcState[Int] =
  input.foldLeft(0.pure[CalcState]) { (a, b) =>
  a.flatMap(_ => evalOne(b))
}

val program = evalAll(List("1", "2", "+", "3", "*"))
program.runA(Nil).value


val program2 = for {
  _   <- evalAll(List("1", "2", "+"))
  _   <- evalAll(List("3", "4", "+"))
  ans <- evalOne("*")
} yield ans

program2.runA(Nil).value

def evalInput(input: String): Int = evalAll(input.split(" ").toList).runA(Nil).value