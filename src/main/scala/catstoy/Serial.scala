package catstoy

trait Serial[A] {
  def serialize(a: A): Array[Byte]
}

object Serial{
  def apply[A](implicit serial: Serial[A]): Serial[A] = serial
  def instance[A](func: A => Array[Byte]): Serial[A] = (a: A) => func(a)
  implicit val stringSerialInstance = instance[String](_.toCharArray.map(_.toByte))
  implicit def listSerialInstance[A](implicit serialA: Serial[A]): Serial[List[A]] =
    instance[List[A]](_.foldLeft(Array[Byte]()){case (bytes, a) => bytes ++ Serial[A].serialize(a)})
}
