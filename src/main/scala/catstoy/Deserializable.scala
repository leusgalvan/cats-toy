package catstoy

import scala.annotation.tailrec

trait Deserializable[A] {
  def deserialize(bytes: Array[Byte]): Option[A]
}

object Deserializable{
  def apply[A](implicit serial: Deserializable[A]): Deserializable[A] = serial
  def instance[A](d: Array[Byte] => Option[A]): Deserializable[A] = new Deserializable[A] {
    override def deserialize(bytes: Array[Byte]): Option[A] = d(bytes)
  }

  val stringMarker = 1.toByte
  implicit val stringDeserialInstance = instance[String] {
    case bs if bs.length <= 1 || bs.head != stringMarker => None
    case bs => Some(new String(bs.slice(2, 2 + bs(1)).map(_.toChar)))
  }

  val listMarker = 2.toByte
  implicit def listDeserialInstance[A](implicit deserializableA: Deserializable[A]): Deserializable[List[A]] = {
    @tailrec
    def read(bytes: Array[Byte], start: Int, readSoFar: List[A]): Option[List[A]] = {
      if(start >= bytes.length) {
        Some(readSoFar.reverse)
      }
      else {
        val end = start + bytes(1) + 2
        deserializableA.deserialize(bytes.drop(start)) match {
          case None => None
          case Some(next) => read(bytes, end, next :: readSoFar)
        }
      }
    }

    instance[List[A]] {
      case bs if bs.length <= 1 || bs.head != listMarker => None
      case bs => read(bs, 2, Nil)
    }
  }
}