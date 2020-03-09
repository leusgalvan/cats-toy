package catstoy

object Main {
  def main(args: Array[String]): Unit = {
    App
      .run()
      .fold(
        ex => println(s"Application ended with error: $ex"),
        _ => println("Application ended successfully")
      )
  }
}
