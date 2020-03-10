package catstoy

import java.io.IOException
import java.nio.file.{Files, Paths, StandardOpenOption}

import cats._
import catstoy.Model.Instances._
import catstoy.Model._
import catstoy.Persistable.Instances._

trait IO[F[_]] {
  def saveUserToFile(user: User, filename: String): F[Unit]
  def readUsersFromFile(filename: String): F[List[User]]
  def deleteAllUsers(filename: String): F[Boolean]
}

object IO {
  def apply[F[_]](
    isFlaky: Boolean
  )(implicit ap: ApplicativeError[F, Throwable]): IO[F] =
    new IO[F] {

      private def pureOrThrow[A](op: A): F[A] = {
        if (isFlaky && math.random() < 0.5) {
          println("Failing...")
          ap.raiseError(new Exception("Random failure"))
        } else ap.pure(op)
      }

      def saveUserToFile(user: User, filename: String): F[Unit] = {
        try {
          pureOrThrow(
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

      def readUsersFromFile(filename: String): F[List[User]] = {
        println("Reading users...")
        val path = Paths.get(filename)
        if (!Files.exists(path)) Files.createFile(path)
        try {
          val readUsers = Files.readAllBytes _ andThen Persistable[List[User]].deserialize _
          readUsers(path) match {
            case Some(users) => pureOrThrow(users)
            case None        => ap.raiseError(new IOException("Serialization error"))
          }
        } catch {
          case e: Throwable => ap.raiseError(e)
        }
      }

      def deleteAllUsers(filename: String): F[Boolean] = {
        try {
          pureOrThrow(Files.deleteIfExists(Paths.get(filename)))
        } catch {
          case e: Throwable => ap.raiseError(e)
        }
      }
    }
}
