package catstoy

import cats._
import cats.implicits._
import cats.data._

object Scheduler {
  def retry[M[_], E, A](m: M[A], times: Int)(implicit monadErrorEv: MonadError[M, E]): M[A] = {
    if(times <= 0) m
    else MonadError[M,E].handleErrorWith(m)(_ => retry(m, times-1))
  }
}

object Model {
  case class User(username: String, password: String, email: String)
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
    Validated.condNec(isValid(email), email, InvalidEmailAddressFormatException)
  }

  def createUser(username: String, password: String, email: String): ValidatedNec[UserDataException, User] = {
    (checkUsernameLength(username),
      checkPasswordContainsUppercase(password),
      checkEmailFormat(email)).mapN { case (username, password, email) =>
        User(username, password, email)
    }
  }
}