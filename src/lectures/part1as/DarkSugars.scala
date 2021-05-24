package lectures.part1as

import scala.util.Try

object DarkSugars extends App{

  // syntax sugar #1: method with single param
  def singleArgMethod(arg: Int): String = s"$arg little ducks"

  val description = singleArgMethod {
    // write some complex code
    42
  }

  val aTryInstance = Try {
    // java's try {...}
    throw new RuntimeException
  }

  List(1,2,3).map { x =>
    x + 1
  }

  // syntax sugar #2: single abstract method
  // trait w/ single method can be converted
  trait Action {
    def act(x: Int): Int
  }

  val anInstance = new Action {
    override def act(x: Int): Int = x + 1
  }

  val aFunkyInstance: Action = (x: Int) => x + 1 // magic

  // example: runnables
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("Hello, Scala")
  })

  val aSweeterThread = new Thread(() => println("Hello, Scala"))

  abstract class AnAbstractType {
    def implemented: Int = 23
    def f(a: Int): Unit
  }

  val anAbstractInstance: AnAbstractType = (a: Int) => println("sweet")

  // syntax sugar #4: the :: and #:: methods are special

  val preprendedList = 2 :: List(3,4)
  // 2.::(List(3,4)) we may think it is this
  // it is actually List(3,4).::(2)

  // scala spec: last char decides associativity of method
  // : as last char means right associativity instead of left
  1 :: 2 :: 3 :: List(4,5)
  List(4,5).::(3).::(2).::(1) // equivalent

  class MyStream[T] {
    def -->:(value: T): MyStream[T] = this // actual implementation here
  }

  val myStream = 1 -->: 2 -->: 3 -->: new MyStream[Int]

  // syntax sugar #4: multi-word method naming

  class TeenGirl(name: String) {
    def `and then said`(gossip: String): Unit = println(s"$name said $gossip")
  }

  val lily = new TeenGirl("Lily")
  lily `and then said` "Scala is great"

  // syntax sugar #5 infix types
  class Composite[A,B]
  val composite: Int Composite String = ???
  // instead of val composite: Composite[Int, String]

  class -->[A,B]
  val towards: Int --> String = ???

  // syntax sugar #6: update() is very special, much like apply()
  val anArray = Array(1,2,3)
  anArray(2) = 7 // rewritted to anArray.update(2,7)
  // used in mutable collections

  // syntax sugar #7: setters for mutable containers
  class Mutable {
    private var internalMember: Int = 0 // private for OO encapsulation
    def member = internalMember // "getter"
    def member_=(value: Int): Unit =
      internalMember = value // "setter"
  }

  val aMutableContainer = new Mutable
  aMutableContainer.member = 42 // rewritten as aMutableContainer.member_=(42)

}
