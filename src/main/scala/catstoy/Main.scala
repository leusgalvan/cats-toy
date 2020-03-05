package catstoy

import java.io.{BufferedOutputStream, BufferedWriter, FileOutputStream, IOException}

import cats._
import cats.implicits._
import cats.data._

object Scheduler {
  def retry[M[_], E, A](m: M[A], times: Int)(implicit applicativeErrorEv: ApplicativeError[M, E]): M[A] = {
    if(times <= 0) m
    else ApplicativeError[M,E].handleErrorWith(m)(_ => retry(m, times-1))
  }
}

object Model {
  import Serialization._

  case class User(username: String, password: String, email: String)

  implicit val userSerialInstance = Serial.instance[User] { user =>
    Serial[NonEmptyChain[User]].serialize(NonEmptyChain(user.username, user.email, user.password))
  }
}

object Form {
  import Model._

  sealed abstract class UserDataException(message: String) extends Exception(message)
  case class UsernameTooShortException(actualLength: Int, minimumLength: Int) extends UserDataException (
    s"Username is too short. Actual length is $actualLength but minimum length required is $minimumLength")
  case object PasswordDoesNotContainUppercaseException extends UserDataException(
    "Password must contain at least one uppercase letter")
  case object InvalidEmailAddressFormatException extends UserDataException(
    "Email address format is invalid"
  )

  private def checkUsernameLength(username: String): ValidatedNec[UserDataException, String] = {
    val actualLength = username.length
    val minimumLength = 8
    if(actualLength < minimumLength) Validated.invalidNec(UsernameTooShortException(actualLength, minimumLength))
    else Validated.validNec(username)
  }

  private def checkPasswordContainsUppercase(password: String): ValidatedNec[UserDataException, String] = {
    Validated.condNec(!password.exists(_.isUpper), password, PasswordDoesNotContainUppercaseException)
  }

  private def checkEmailFormat(email: String): ValidatedNec[UserDataException, String] = {
    def isValid(email: String) = {
      val emailRegex = """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r
      email match{
        case null                                           => false
        case e if e.trim.isEmpty                            => false
        case e if emailRegex.findFirstMatchIn(e).isDefined  => true
        case _                                              => false
      }
    }
    Validated.valid(email).ensure(InvalidEmailAddressFormatException)(isValid).toValidatedNec
  }

  def createUser(username: String, password: String, email: String): ValidatedNec[UserDataException, User] = {
    (checkUsernameLength(username),
      checkPasswordContainsUppercase(password),
      checkEmailFormat(email)).mapN { case (username, password, email) =>
        User(username, password, email)
    }
  }
}

object Serialization {
  trait Serial[A] {
    def serialize(a: A): Array[Byte]
  }

  object Serial{
    def apply[A](implicit serial: Serial[A]): Serial[A] = serial
    def instance[A](func: A => Array[Byte]): Serial[A] = (a: A) => func(a)

    implicit val stringSerialInstance = instance[String](_.toCharArray.map(_.toByte))

    implicit def foldableSerialInstance[F[_]: Reducible, A: Semigroup](implicit serialA: Serial[A]) =
      instance[F[A]](Reducible[F].reduce[A] _ andThen serialA.serialize)
  }
}

object IO {
  import Model._
  import Serialization._

  def saveUserToFile[F[_]](user: User, filename: String)(implicit ap: ApplicativeError[F, IOException]): F[Unit] = {
    val outputStream = new BufferedOutputStream(new FileOutputStream(filename, true))
    try {
      ap.pure(outputStream.write(Serial[User].serialize(user)))
    } catch {
      case e: IOException => ap.raiseError(e)
    } finally {
      outputStream.close()
    }
  }
}