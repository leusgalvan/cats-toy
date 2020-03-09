package catstoy

import Persistable.PersistableTests
import org.specs2.mutable.Specification
import org.typelevel.discipline.specs2.mutable.Discipline
import catstoy.Persistable.Instances._
import cats.implicits._
import org.scalacheck.{Arbitrary, Gen}

class PersistableSpec extends Specification with Discipline {
  implicit val arbitraryString = Arbitrary(
    Gen.asciiStr.suchThat(_.length < 256)
  )

  "Persistable" should {
    "satisfy laws for instances" in {
      checkAll("String", PersistableTests[String].persistable)
      checkAll("List[String]", PersistableTests[List[String]].persistable)
    }
  }
}
