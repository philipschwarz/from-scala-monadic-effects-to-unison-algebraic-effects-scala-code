object Main extends App {

  sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] =
      this flatMap { a => Some(f(a)) }

    def flatMap[B](f: A => Option[B]): Option[B] =
      this match {
        case Some(a) => f(a)
        case None => None
      }

  }
  case object None extends Option[Nothing]
  case class Some[+A](get: A) extends Option[A]

  case class Person(name: String, surname: String, age: Int)

  def toText(option: Option[Person]): String =
    option match {
      case Some(person) => person.toString
      case None => "None"
    }

  def validateName(name: String): Option[String] =
    if (name.size > 1 && name.size < 15)
      Some(name)
    else None

  def validateSurname(surname: String): Option[String] =
    if (surname.size > 1 && surname.size < 20)
      Some(surname)
    else None

  def validateAge(age: Int): Option[Int] =
    if (age > 0 && age < 112)
      Some(age)
    else None

  def createPerson(name : String, surname: String, age: Int): Option[Person] =
    for {
      aName    <- validateName(name)
      aSurname <- validateSurname(surname)
      anAge    <- validateAge(age)
    } yield Person(aName, aSurname, anAge)

  def createPerson2(name : String, surname: String, age: Int): Option[Person] =
      validateName(name) flatMap { aName =>
        validateSurname(surname) flatMap { aSurname =>
          validateAge(age) map { anAge =>
            Person(aName, aSurname, anAge)
          }
        }
      }

  val potentialPeople: List[Option[Person]] = List(
    createPerson("Fred", "Smith", 35),
    createPerson(   "x", "Smith", 35),
    createPerson("Fred",      "", 35),
    createPerson("Fred", "Smith",  0)
  )

  val people: String = potentialPeople.foldLeft("")(((text,person) => text + "\n" + toText(person)))

  assert( people == "\nPerson(Fred,Smith,35)\nNone\nNone\nNone" )

  println(people)

}
