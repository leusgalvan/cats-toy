package catstoy

import java.io.{BufferedOutputStream, FileOutputStream, IOException}

import cats._

object IO {
  import Model._

  def saveUserToFile[F[_]](user: User, filename: String)(implicit ap: ApplicativeError[F, IOException]): F[Unit] = {
    val outputStream = new BufferedOutputStream(new FileOutputStream(filename, true))
    try {
      ap.pure(outputStream.write(Serializable[User].serialize(user)))
    } catch {
      case e: IOException => ap.raiseError(e)
    } finally {
      outputStream.close()
    }
  }
}