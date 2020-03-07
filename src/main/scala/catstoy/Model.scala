package catstoy

import Serializable.Instances._
import Deserializable.Instances._
import cats.kernel.Eq

object Model {
  case class User(username: String, password: String, email: String)

  object Instances {
    val userMarker = 3.toByte
    implicit val userPersistableInstance = Persistable.instance[User] { user =>
      val usernameBytes = Serializable[String].serialize(user.username)
      val passwordBytes = Serializable[String].serialize(user.password)
      val emailBytes = Serializable[String].serialize(user.email)
      val totalLength = usernameBytes.length + passwordBytes.length + emailBytes.length
      Array(userMarker, totalLength.toByte) ++ usernameBytes ++ passwordBytes ++ emailBytes
    } {
      case bs if bs.length <= 1 || bs.head != userMarker => None
      case bs =>
        for {
          username <- Deserializable[String].deserialize(bs.drop(2))
          password <- Deserializable[String].deserialize(bs.drop(4 + username.length))
          email <- Deserializable[String].deserialize(bs.drop(6 + username.length + password.length))
        } yield User(username, password, email)
    }

    implicit val userEqInstance = Eq.instance[User]( _.username == _.username )
  }

}
