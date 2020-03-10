package catstoy

import java.util.NoSuchElementException

import cats.implicits._
import cats.{MonadError, Show}
import catstoy.Model.User

import scala.util.control.NonFatal

trait App[F[_]] {
  def run(): F[Unit]
}

object App {
  def apply[F[_]](implicit ap: MonadError[F, Throwable]) = new App[F] {
    val CREATE_USER_OPCODE = 1
    val LIST_USERS_OPCODE = 2
    val DELETE_USERS_OPCODE = 3
    val EXIT_OPCODE = 4
    val ALL_OPCODES =
      Set(
        CREATE_USER_OPCODE,
        LIST_USERS_OPCODE,
        DELETE_USERS_OPCODE,
        EXIT_OPCODE
      )

    val io = IO[F](isFlaky = true)

    private def withRetries[A](op: F[A]) =
      Scheduler.retry[F, Throwable, A](() => op, 5, Some("\n\nRetrying..."))

    private def readOperation(): F[Int] = {
      print("""
          |What do you want to do?
          | (1) Create a user
          | (2) List all users
          | (3) Delete all users
          | (4) Exit
          | 
          | Your answer: """.stripMargin)

      val opCode = scala.io.StdIn.readLine().toInt
      if (ALL_OPCODES.contains(opCode)) ap.pure(opCode)
      else ap.raiseError(new Exception(s"Unknown operation: $opCode"))
    }

    private def readString(label: String): F[String] = {
      print(label)
      ap.pure(scala.io.StdIn.readLine())
    }

    private def createUser(): F[Unit] = {
      import Model.Instances.userUnsafeShowInstance
      import Instances.throwableShow
      import Instances.necStringMultilineShow

      for {
        username <- readString("\n\nPlease enter username: ")
        password <- readString("\nPlease enter password: ")
        email <- readString("\nPlease enter email: ")
        newUser <- Form
          .createUser(username, password, email)
          .leftMap(exceptions => new Exception(exceptions.show))
          .liftTo[F]
        _ <- withRetries(io.saveUserToFile(newUser, "users.txt"))

      } yield {
        println(s"\n\nUser successfully created: ${newUser.show}\n")
      }
    }

    private def listUsers(): F[Unit] = {
      import Model.Instances.userUnsafeShowInstance
      implicit val listShow: Show[List[User]] =
        Instances.listStringMultilineShow

      for {
        users <- withRetries(io.readUsersFromFile("users.txt"))
      } yield {
        println(s"\n\nUsers currently in database:\n ${users.show}\n")
      }
    }

    private def deleteAllUsers(): F[Unit] = {
      for {
        _ <- withRetries(io.deleteAllUsers("users.txt"))
      } yield {
        println(s"\n\nUsers deleted succesfully")
      }
    }

    case object PleaseExitException extends Exception

    private def exit(): F[Unit] = ap.raiseError(PleaseExitException)

    private def executeOperation(operationCode: Int): F[Unit] = {
      operationCode match {
        case CREATE_USER_OPCODE  => createUser()
        case LIST_USERS_OPCODE   => listUsers()
        case DELETE_USERS_OPCODE => deleteAllUsers()
        case EXIT_OPCODE         => exit()
      }
    }

    override def run(): F[Unit] = {
      val program = for {
        operation <- readOperation()
        _ <- executeOperation(operation)
        _ <- run()
      } yield ()

      program.recoverWith {
        case PleaseExitException =>
          ap.pure(println("\n\nBye :)"))
        case _: NoSuchElementException =>
          println("\n\nUnrecognized operation code")
          run()
        case NonFatal(e) =>
          println(s"\n\nAn error ocurred: ${e.getMessage}")
          run()
      }
    }
  }
}
