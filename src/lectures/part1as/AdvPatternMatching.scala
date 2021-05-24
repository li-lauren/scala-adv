package lectures.part1as

object AdvPatternMatching extends App{
  val numbers = List(1)
  val description = numbers match {
    case head :: Nil => println(s"the only element is $head")
    case _ =>
  }
  /*
    - constants
    - wildcards
    - case classes
    - tuples
    - some special magic like above
   */

  // custom pattern matching
  class Person(val name: String, val age: Int)

  // create a singleton object ; in this case an extractor object
  object Person {
    def unapply(person: Person): Option[(String, Int)] =
      if (person.age < 21) None
      else Some((person.name, person.age))

    def unapply(age: Int): Option[String] =
      Some(if (age < 21) "minor" else "major")
  }

  val bob = new Person("Bob", 25)
  val greeting = bob match {
    case Person(n, a) => s"Hi, my name is $n and I am $a yo."
      // look for method unapply in object called Person and that returns a tuple with two things
  }

  val legalStatus = bob.age match {
    case Person(status) => s"My leagl status is $status"
  }

  /*
    Exercise
   */

//  object even {
//    def unapply(arg: Int): Option[Boolean] =
//      if (arg % 2 == 0) Some(true)
//      else None
//  }
   object even {
     def unapply(arg: Int): Boolean = arg % 2 == 0
   }
//
//  object singleDigit {
//    def unapply(arg: Int): Option[Boolean] =
//      if (arg > -10 && arg < 10) Some(true)
//      else None
//  }
  object singleDigit {
    def unapply(arg: Int): Boolean = arg > -10 && arg < 10
  }
  val n: Int = 45
  val mathProperty = n match {
    case singleDigit() => "Single digit"
    case even() => "an even number"
    case _ => "no property"
  }

  // infix patterns
  case class Or[A,B](a: A, b: B)
  val either = Or(2, "two")
  val humanDescription = either match {
      // case Or(number, string)
    case number Or string => s"$number is written as $string"
  }

  // decomposing sequences
  val vararg = numbers match {
    case List(1, _*) => "starting with 1"
  }

  // small list implementation
  abstract class MyList[+A] {
    def head: A = ???
    def tail: MyList[A] = ???
  }
  case object Empty extends MyList[Nothing]
  case class Cons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  object MyList {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if (list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _)
  }

  val myList: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val decomposed = myList match {
    case MyList(1, 2, _*) => "starting with 1,2"
      // compiler looks in to object MyList and looks for unapplySeq
    case _ => "something else"
  }

  // custom return types for unapply
  // return data type isn't limited to Option
  // just need to have isEmtpy: Boolean and get: something methods

  abstract class Wrapper[T] {
    def isEmpty: Boolean
    def get: T
  }

  object PersonWrapper {
    def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
      def isEmpty = false
      def get = person.name
    }
  }

  println(bob match {
    case PersonWrapper(n) => s"Person's name is $n"
    case _ => "Person not found"
  })
}
