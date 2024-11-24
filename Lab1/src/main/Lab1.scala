
//2
def multipleHellos(n: Int): Unit = {for (i <- 1 to n){if (i % 2 == 0) {println(s"hello $i")}else{println(s"hello ${n - i}")}}}

//3a
def returnMaxElement(numbers: Seq[Int]): Int = {
  numbers.reduce((a, b) => if (a > b) a else b)
}

//3b
def splitByIndex(numbers: Seq[Int]): (Seq[Int], Seq[Int]) = {
  val evenIndexNumber = numbers.zipWithIndex.filter((_, index) => index%2 == 0).map(_._1)
  val oddIndexNumber = numbers.zipWithIndex.filter((_, index) => index%2 != 0).map(_._1)

  (evenIndexNumber,oddIndexNumber)
}

//5
def describeType(x: Any): String = x match {
  case i: Int => s"It's Integer: $i"
  case s: String => s"It's String: $s"
  case d: Double => s"It's Double: $d"
  case _ => "Unknown type"
}

//6
def compose[A, B, C](f: B => C, g: A => B): A => C = {
    x => f(g(x))
}

@main def starLab1() = {

  //1
  println("\nTask1)")

  println("Hello world")

  //2
  println("\nTask2)")

  multipleHellos(9)

  //3
  println("\nTask3)")


  val nums = Seq(-1,0,10,45,65)

  //3a
   println("\na)")

  println("Maximum humber of sequence - " + returnMaxElement(nums) + ". Sequence - " + nums)

  //3b
  println("\nb)")

  val (evenIndexNumber,oddIndexNumber) = splitByIndex(nums)
  println("Sequence:" + nums)
  println("Numbers of sequence with even indexes: " + evenIndexNumber)
  println("Numbers of sequence with odd indexes: " + oddIndexNumber)
 
  //4
  println("\nTask4)")

  val maxElem = returnMaxElement
  println(maxElem)
  //Вернет информацию о имени объекта , которую компилятор создал для хранения данной функции и хеш-код объекта функции
  

  //5
  println("\nTask5)")

  println(describeType(52))        
  println(describeType("I like trains"))   
  println(describeType(19.84))      
  println(describeType(false))    

  //6
  println("\nTask6)")

  val secondExponentiation: Int =>Int = x => x*x
  val oddByPi: Int =>Double = x =>x/3.14

  println(compose(oddByPi,secondExponentiation)(3))

}