package catstoy

import java.io.{FileNotFoundException, IOException}
import java.nio.file.{Files, Paths, StandardOpenOption}

import cats._
import catstoy.Model.Instances._
import catstoy.Model._
import catstoy.Persistable.Instances._

object IO {
  def saveUserToFile[F[_]](user: User, filename: String)(
    implicit ap: ApplicativeError[F, Throwable]
  ): F[Unit] = {
    try {
      ap.pure(
        Files.write(
          Paths.get(filename),
          Persistable[User].serialize(user),
          StandardOpenOption.CREATE,
          StandardOpenOption.APPEND
        )
      )
    } catch {
      case e: Throwable => ap.raiseError(e)
    }
  }

  def readUsersFromFile[F[_]](
    filename: String
  )(implicit ap: ApplicativeError[F, Throwable]): F[List[User]] = {
    val path = Paths.get(filename)
    if (!Files.exists(path)) Files.createFile(path)
    try {
      val readUsers = Files.readAllBytes _ andThen Persistable[List[User]].deserialize _
      readUsers(path) match {
        case Some(users) => ap.pure(users)
        case None        => ap.raiseError(new IOException("Serialization error"))
      }
    } catch {
      case e: Throwable => ap.raiseError(e)
    }
  }

  def deleteAllUsers[F[_]](
    filename: String
  )(implicit ap: ApplicativeError[F, Throwable]): F[Boolean] = {
    try {
      ap.pure(Files.deleteIfExists(Paths.get(filename)))
    } catch {
      case e: Throwable => ap.raiseError(e)
    }
  }
}
