import cats.data.Reader
import cats.syntax.applicative._

case class Db(usernames: Map[Int, String],
              passwords: Map[String, String])

type DbReader[X] = Reader[Db, X]

def findUsername(userId: Int): DbReader[Option[String]] = Reader(db => db.usernames.get(userId))
def checkPassword(
                   username: String,
                   password: String): DbReader[Boolean] = Reader(db => db.passwords.get(username).contains(password))

def checkLogin(userId: Int,
               password: String): DbReader[Boolean] = for {
  user <- findUsername(userId)
  result <- user.map(userName => checkPassword(userName, password))
    .getOrElse(false.pure[DbReader])
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
// res10: cats.Id[Boolean] = true
checkLogin(4, "davinci").run(db)
// res11: cats.Id[Boolean] = false