import scala.util.{Try, Success, Failure}
import scala.concurrent.{Future,Await}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.StdIn

//1

def integral(f: Double => Double, l: Double, r: Double, steps: Int): Double = {
  val stepSize = (r - l) / steps
  val points = (1 to steps).map(i => f(l + (i + 0.5) * stepSize))
  points.map(f).reduce(_ + _) * stepSize
}

//2
def goodEnoughPassword(password: String): Option[Boolean] = {
  val checkpass:List[String =>Boolean]= List(
    _.length >= 8,                         
    _.exists(_.isUpper),                   
    _.exists(_.isLower),                   
    _.exists(_.isDigit),                  
    _.exists("!@#$%^&*()_+".contains(_))
  )
  Option(password).filter(pwd => checkpass.forall(check => check(pwd))).map(_ => true)                                     
}

//3
import scala.util.{Try, Success, Failure}

def goodEnoughPasswordTry(password: String): Either[Boolean, String] = {
  Try {
    require(password != null, "Password must not be null")
    password
  } match {
    case Success(pwd) =>
      val checkpass: List[(String, String => Boolean)] = List(
        ("Length >= 8 symbols", (password: String) => password.length >= 8),
        ("Minimum 1 number", (password: String) => password.exists(_.isDigit)),
        ("Minimum 1 upper letter", (password: String) => password.exists(_.isUpper)),
        ("Minimum 1 lower letter", (password: String) => password.exists(_.isLower)),
        ("Minimum 1 special symbol", (password: String) => password.exists("!@#$%^&*()_+".contains(_)))
      )

      val failures = checkpass.collect { case (msg, checkpass) if !checkpass(pwd) => msg }

      if (failures.isEmpty) Left(true)                  
      else Right(s"Password not matching requirements: ${failures.mkString(", ")}") 

    case Failure(_) => Right("Indentified error.")
  }
}

//4
def readPassword(): Future[String] = {
  Future {
    var password: Option[String] = None
    while (password.isEmpty) {
      println("Введите пароль:")
      val input = StdIn.readLine()

      goodEnoughPasswordTry(input) match {
        case Left(true) =>
          println("Пароль подходит!")
          password = Some(input) 

        case Right(reason) =>
          println(s"Пароль не принят: $reason. Попробуйте ещё раз.")
      }
    }
    password.get 
  }
}

@main def startLab2() = {
    //1
    println("\nTask1)")

    val result = integral(x => x * x, 0, 1, 20)
    println(result)

    //2
    println("\nTask2)")

    println(goodEnoughPassword(null))
    println(goodEnoughPassword("Abc12345"))       
    println(goodEnoughPassword("Password!1"))    
    println(goodEnoughPassword("Pass!"))         
    println(goodEnoughPassword("12345678!"))     
    println(goodEnoughPassword("GoodP@ssw0rd"))

    //3
    println("\nTask3)")

    println(goodEnoughPasswordTry(null))
    println(goodEnoughPasswordTry("Abc12345"))       
    println(goodEnoughPasswordTry("Password!1"))    
    println(goodEnoughPasswordTry("Pass!"))         
    println(goodEnoughPasswordTry("12345678!"))     
    println(goodEnoughPasswordTry("GoodP@ssw0rd"))

    //4
    println("\nTask4)")

    val resultFuture = readPassword()
    val resultpass = Await.result(resultFuture, Duration.Inf) 
    println(s"Успешно введён пароль: $resultpass")      
}

