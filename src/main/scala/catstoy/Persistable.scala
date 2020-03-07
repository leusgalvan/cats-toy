package catstoy

import cats.Eq
import cats.laws._
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws
import cats.laws.discipline.catsLawsIsEqToProp
import cats.instances.option._

trait Persistable[A] extends Serializable[A] with Deserializable[A]

object Persistable {
  def apply[A](implicit persistable: Persistable[A]): Persistable[A] = persistable
  def instance[A](s: A => Array[Byte])(d: Array[Byte] => Option[A]): Persistable[A] = new Persistable[A] {
    override def serialize(a: A): Array[Byte] = s(a)
    override def deserialize(bytes: Array[Byte]): Option[A] = d(bytes)
  }

  trait PersistableLaws[A] {
    implicit def P: Persistable[A]

    def isomorphism(a: A): IsEq[Option[A]] = (P.serialize _ andThen P.deserialize _)(a) <-> Some(a)
  }

  object PersistableLaws {
    def apply[A: Persistable]: PersistableLaws[A] = new PersistableLaws[A] {
      override def P: Persistable[A] = Persistable[A]
    }
  }

  trait PersistableTests[A] extends Laws {

    def laws: PersistableLaws[A]

    def persistable(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
      new DefaultRuleSet(
        name = "persistable",
        parent = None,
        props = "isomorphism" -> Prop.forAll(laws.isomorphism _)
      )

  }

  object PersistableTests {
    def apply[A: Persistable]: PersistableTests[A] =
      new PersistableTests[A] { def laws: PersistableLaws[A] = PersistableLaws[A] }
  }
}
