package catstoy

import java.nio.ByteBuffer

import scala.annotation.tailrec

trait Serial[A] {
  def serialize(a: A): Array[Byte]
  def deserialize(bytes: Array[Byte]): Option[A]
}

object Serial{
  def apply[A](implicit serial: Serial[A]): Serial[A] = serial
  def instance[A](s: A => Array[Byte])(d: Array[Byte] => Option[A]): Serial[A] = new Serial[A] {
    override def serialize(a: A): Array[Byte] = s(a)
    override def deserialize(bytes: Array[Byte]): Option[A] = d(bytes)
  }

  val stringMarker = 1.toByte
  implicit val stringSerialInstance = instance[String]{ s =>
    val bytes = s.toCharArray.map(_.toByte)
    Array(stringMarker, bytes.length.toByte) ++ bytes
  } {
    case bs if bs.length <= 1 || bs.head != stringMarker => None
    case bs => Some(new String(bs.slice(2, 2 + bs(1)).map(_.toChar)))
  }

  val listMarker = 2.toByte
  implicit def listSerialInstance[A](implicit serialA: Serial[A]): Serial[List[A]] = {
    @tailrec
    def read(bytes: Array[Byte], start: Int, readSoFar: List[A]): Option[List[A]] = {
      if(start >= bytes.length) {
        Some(readSoFar.reverse)
      }
      else {
        val end = start + bytes(1) + 2
        serialA.deserialize(bytes.drop(start)) match {
          case None => None
          case Some(next) => read(bytes, end, next :: readSoFar)
        }
      }
    }

    instance[List[A]] { xs =>
      val bytes = xs.foldLeft(Array[Byte]()) { case (bytes, a) => bytes ++ Serial[A].serialize(a) }
      Array(listMarker, bytes.length.toByte) ++ bytes
    } {
      case bs if bs.length <= 1 || bs.head != listMarker => None
      case bs => read(bs, 2, Nil)
    }
  }
}
