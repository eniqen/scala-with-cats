
import cats.{Eq, Show}
import cats.syntax.show._
import cats.syntax.eq._
import cats.instances.int._
import cats.instances.string._
import cats.instances.option._

final case class Cat(name: String, age: Int, color: String)

implicit val catShow: Show[Cat] = Show.show { cat =>
  val name = cat.name.show
  val age = cat.age.show
  val color = cat.color.show
  s"$name is a $age year-old $color cat."
}

implicit val catEq: Eq[Cat] = Eq.instance { (cat1, cat2) =>
    cat1.name === cat2.name &&
    cat1.age === cat2.age &&
    cat1.color === cat2.color
}

println(Cat("Tom", 3, "red").show)


val cat1 = Cat("Garfield",   38, "orange and black")
val cat2 = Cat("Heathcliff", 33, "orange and black")

val optionCat1 = Option(cat1)
val optionCat2 = Option.empty[Cat]

cat1 === cat2

optionCat1 === optionCat2