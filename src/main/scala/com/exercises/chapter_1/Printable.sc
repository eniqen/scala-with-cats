trait Printable[A] {
  def format(value: A): String
}

final case class Cat(name: String, age: Int, color: String)

object Printable {
  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)

  def print[A](value: A)(implicit p: Printable[A]): Unit =
    println(format(value)(p))
}

object PrintableInstances {

  implicit val stringPrintable: Printable[String] = (value: String) => value

  implicit val intPrintable: Printable[Int] = (value: Int) => value.toString

  implicit val catPrintable: Printable[Cat] = (cat: Cat) => {
    import Printable._
    val name = format(cat.name)
    val age = format(cat.age)
    val color = format(cat.color)
    s"$name is a $age year-old $color cat."
  }
}

object PrintableSyntax {
  implicit class PrintableOps[A](val value: A) extends AnyVal {
    def format(implicit p: Printable[A]): String = {
      p.format(value)
    }

    def print(implicit p: Printable[A]): Unit = println(p.format(value))
  }
}
import PrintableSyntax._
val cat = Cat("Tom", 3, "red")

Printable.print(cat)(PrintableInstances.catPrintable)

import PrintableInstances._
cat.print