package catstoy

import cats.data.{Validated, ValidatedNec}
import cats.implicits._
import Model._

object Form {
  sealed abstract class UserDataException(message: String)
      extends Exception(message)
  case class UsernameTooShortException(actualLength: Int, minimumLength: Int)
      extends UserDataException(
        s"Username is too short. Actual length is $actualLength but minimum length required is $minimumLength"
      )
  case object PasswordDoesNotContainUppercaseException
      extends UserDataException(
        "Password must contain at least one uppercase letter"
      )
  case object InvalidEmailAddressFormatException
      extends UserDataException("Email address format is invalid")

  private def checkUsernameLength(
    username: String
  ): ValidatedNec[UserDataException, String] = {
    val actualLength = username.length
    val minimumLength = 8
    if (actualLength < minimumLength)
      Validated.invalidNec(
        UsernameTooShortException(actualLength, minimumLength)
      )
    else Validated.validNec(username)
  }

  private def checkPasswordContainsUppercase(
    password: String
  ): ValidatedNec[UserDataException, String] = {
    Validated.condNec(
      !password.exists(_.isUpper),
      password,
      PasswordDoesNotContainUppercaseException
    )
  }

  private def checkEmailFormat(
    email: String
  ): ValidatedNec[UserDataException, String] = {
    def isValid(email: String) = {
      val emailRegex =
        """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r
      email match {
        case null                                          => false
        case e if e.trim.isEmpty                           => false
        case e if emailRegex.findFirstMatchIn(e).isDefined => true
        case _                                             => false
      }
    }
    Validated
      .valid(email)
      .ensure(InvalidEmailAddressFormatException)(isValid)
      .toValidatedNec
  }

  def createUser(username: String,
                 password: String,
                 email: String): ValidatedNec[UserDataException, User] = {
    (
      checkUsernameLength(username),
      checkPasswordContainsUppercase(password).combine(
        checkPasswordContainsUppercase(password)
      ),
      checkEmailFormat(email)
    ).mapN {
      case (username, password, email) =>
        User(username, password, email)
    }
  }
}
