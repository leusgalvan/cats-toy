package catstoy

import java.nio.ByteBuffer

object Model {
  case class User(username: String, password: String, email: String)

  val userMarker = 3.toByte
  implicit val userSerialInstance = Serial.instance[User] { user =>
    val bytes = Serial[List[String]].serialize(List(user.username, user.email, user.password))
    Array(userMarker, bytes.length.toByte) ++
      Serial[String].serialize(user.username) ++
      Serial[String].serialize(user.password) ++
      Serial[String].serialize(user.email)
  }
  {
    case bs if bs.length <= 1 || bs.head != userMarker => None
    case bs =>
      for {
        username <- Serial[String].deserialize(bs.drop(2))
        password <- Serial[String].deserialize(bs.drop(4 + username.length))
        email    <- Serial[String].deserialize(bs.drop(6 + username.length + password.length))
      } yield User(username, password, email)
  }

}
