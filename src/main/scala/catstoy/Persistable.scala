package catstoy

trait Persistable[A] extends Serializable[A] with Deserializable[A]

object Persistable {
  def apply[A](implicit persistable: Persistable[A]): Persistable[A] = persistable
  def instance[A](s: A => Array[Byte])(d: Array[Byte] => Option[A]): Persistable[A] = new Persistable[A] {
    override def serialize(a: A): Array[Byte] = s(a)
    override def deserialize(bytes: Array[Byte]): Option[A] = d(bytes)
  }
}
