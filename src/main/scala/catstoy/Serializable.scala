package catstoy

trait Serializable[A] {
  def serialize(a: A): Array[Byte]
}

object Serializable {
  def apply[A](implicit serial: Serializable[A]): Serializable[A] = serial
  def instance[A](s: A => Array[Byte]): Serializable[A] = new Serializable[A] {
    override def serialize(a: A): Array[Byte] = s(a)
  }

  object Instances {
    val stringMarker = 1.toByte
    implicit val stringSerialInstance = instance[String] { s =>
      val bytes = s.toCharArray.map(_.toByte)
      Array(stringMarker, bytes.length.toByte) ++ bytes
    }
    implicit def listSerialInstance[A](
      implicit serialA: Serializable[A]
    ): Serializable[List[A]] = {
      instance[List[A]] {
        _.foldLeft(Array[Byte]()) {
          case (bytes, a) => bytes ++ Serializable[A].serialize(a)
        }
      }
    }
  }
}
