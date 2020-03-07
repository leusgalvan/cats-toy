import catstoy.Model.User
import catstoy.Persistable.PersistableTests
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.mutable.Specification
import catstoy.Model.Instances._
import org.typelevel.discipline.specs2.mutable.Discipline

class UserSpec extends Specification with Discipline {
  implicit val genEmail: Gen[String] = for {
    user <- Gen.alphaNumStr
    host <- Gen.alphaNumStr
    extension <- Gen.oneOf("com", "org", "net")
  } yield s"$user@$host.$extension"

  implicit val arbitraryUser: Arbitrary[User] = Arbitrary {
    for {
      username <- Gen.alphaNumStr
      password <- Gen.asciiStr
      email <- genEmail
    } yield User(username, password, email)
  }

  "User" should {
    "satisfy persistable laws" in {
      checkAll("User", PersistableTests[User].persistable)
    }
  }
}
