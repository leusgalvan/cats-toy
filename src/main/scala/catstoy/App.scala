package catstoy

import cats.implicits._
import Model.Instances.userSafeShowInstance

import scala.util.Try

object App {
  val CREATE_USER_OPCODE = 1
  val LIST_USERS_OPCODE = 2
  val ALL_OPCODES = Set(CREATE_USER_OPCODE, LIST_USERS_OPCODE)

  private def readOperation(): Try[Int] = {
    println("""What do you want to do?
        | (1) Create a user
        | (2) List all users
        | 
        | Your answer: 
        |""".stripMargin)

    Try(scala.io.StdIn.readLine().toInt).filter(ALL_OPCODES.contains)
  }

  private def readString(label: String): Try[String] = {
    print(label)
    Try(scala.io.StdIn.readLine())
  }

  private def createUser(): Try[Unit] = {
    for {
      username <- readString("Please enter username: ")
      password <- readString("Please enter password: ")
      email <- readString("Please enter email: ")
      newUser <- Form
        .createUser(username, password, email)
        .leftMap(exceptions => new Exception(exceptions.map(_.getMessage).show))
        .liftTo[Try]
      _ <- IO.saveUserToFile[Try](newUser, "users.txt")

    } yield ()
  }

  private def listUsers(): Try[Unit] = {
    for {
      users <- IO.readUsersFromFile[Try]("users.txt")
    } yield {
      println(s"Users currently in database:\n ${users.show}")
    }
  }

  def run(): Try[Unit] = {
    for {
      operation <- readOperation()
      _ <- operation match {
        case CREATE_USER_OPCODE => createUser()
        case LIST_USERS_OPCODE  => listUsers()
      }
      _ <- run()
    } yield ()
  }
}
