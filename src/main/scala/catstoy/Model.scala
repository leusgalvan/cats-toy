package catstoy

import Persistable.Instances._
import cats.Show
import cats.kernel.Eq

object Model {
  case class User(username: String, password: String, email: String)

  object Instances {
    val userMarker = 3.toByte
    implicit val userPersistableInstance = Persistable.instance[User] { user =>
      val usernameBytes = Persistable[String].serialize(user.username)
      val passwordBytes = Persistable[String].serialize(user.password)
      val emailBytes = Persistable[String].serialize(user.email)
      val totalLength = usernameBytes.length + passwordBytes.length + emailBytes.length
      Array(userMarker, totalLength.toByte) ++ usernameBytes ++ passwordBytes ++ emailBytes
    } {
      case bs if bs.length <= 1 || bs.head != userMarker => None
      case bs =>
        for {
          username <- Persistable[String].deserialize(bs.drop(2))
          password <- Persistable[String].deserialize(
            bs.drop(4 + username.length)
          )
          email <- Persistable[String].deserialize(
            bs.drop(6 + username.length + password.length)
          )
        } yield User(username, password, email)
    }

    implicit val userEqInstance = Eq.instance[User](_.username == _.username)
    implicit val userSafeShowInstance = Show.show[User](
      user =>
        s"User(username=${user.username} password=******** email=${user.email})"
    )
    implicit val userUnsafeShowInstance = Show.show[User](
      user =>
        s"User(username=${user.username} password=${user.password} email=${user.email})"
    )
  }

}
