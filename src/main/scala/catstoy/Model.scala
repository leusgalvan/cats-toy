package catstoy

object Model {
  case class User(username: String, password: String, email: String)

  implicit val userSerialInstance = Serial.instance[User] { user =>
    Serial[List[String]].serialize(List(user.username, user.email, user.password))
  }
}
