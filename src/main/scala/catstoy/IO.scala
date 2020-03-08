package catstoy

import java.io.IOException
import java.nio.file.{Files, Paths, StandardOpenOption}

import cats._
import catstoy.Model.Instances._
import catstoy.Model._
import catstoy.Deserializable.Instances._

object IO {
  def saveUserToFile[F[_]](user: User, filename: String)(
    implicit ap: ApplicativeError[F, IOException]
  ): F[Unit] = {
    try {
      ap.pure(
        Files.write(
          Paths.get(filename),
          Serializable[User].serialize(user),
          StandardOpenOption.APPEND
        )
      )
    } catch {
      case e: IOException => ap.raiseError(e)
    }
  }

  def readUsersFromFile[F[_]](
    filename: String
  )(implicit ap: ApplicativeError[F, IOException]): F[List[User]] = {
    try {
      val path = Paths.get(filename)
      val readUsers = Files.readAllBytes _ andThen Deserializable[List[User]].deserialize _
      readUsers(path) match {
        case Some(users) => ap.pure(users)
        case None        => ap.raiseError(new IOException("Serialization error"))
      }
    } catch {
      case e: IOException => ap.raiseError(e)
    }
  }
}
