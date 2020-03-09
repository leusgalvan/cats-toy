package catstoy

import java.nio.charset.StandardCharsets

import cats.Eq
import cats.laws._
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws
import cats.laws.discipline.catsLawsIsEqToProp
import cats.instances.option._

import scala.annotation.tailrec

trait Persistable[A] {
  def serialize(a: A): Array[Byte]
  def deserialize(bytes: Array[Byte]): Option[A]
}

object Persistable {
  def apply[A](implicit persistable: Persistable[A]): Persistable[A] =
    persistable
  def instance[A](
    s: A => Array[Byte]
  )(d: Array[Byte] => Option[A]): Persistable[A] = new Persistable[A] {
    def serialize(a: A): Array[Byte] = s(a)
    def deserialize(bytes: Array[Byte]): Option[A] = d(bytes)
  }

  object Instances {
    val stringMarker = 1.toByte
    implicit val stringPersistableInstance = instance[String] { s =>
      val bytes = s.getBytes()
      Array(stringMarker, bytes.length.toByte) ++ bytes
    } {
      case bs if bs.length <= 1 || bs.head != stringMarker => None
      case bs                                              => Some(new String(bs.slice(2, 2 + bs(1))))
    }

    implicit def listPersistableInstance[A: Persistable]
      : Persistable[List[A]] = {
      instance[List[A]] {
        _.foldLeft(Array[Byte]()) {
          case (bytes, a) => bytes ++ Persistable[A].serialize(a)
        }
      } { bytes =>
        @tailrec
        def read(start: Int, readSoFar: List[A]): Option[List[A]] = {
          if (start >= bytes.length) {
            Some(readSoFar.reverse)
          } else {
            val nextPos = start + bytes(start + 1) + 2
            Persistable[A].deserialize(bytes.drop(start)) match {
              case None       => None
              case Some(next) => read(nextPos, next :: readSoFar)
            }
          }
        }

        read(0, Nil)
      }
    }
  }

  trait PersistableLaws[A] {
    implicit def P: Persistable[A]

    def isomorphism(a: A): IsEq[Option[A]] =
      (P.serialize _ andThen P.deserialize _)(a) <-> Some(a)
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
      new PersistableTests[A] {
        def laws: PersistableLaws[A] = PersistableLaws[A]
      }
  }
}
