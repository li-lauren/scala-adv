package lectures.part2afp

object CurriesPAF extends App {

  // curried functions
  // functions returning other functions as results
  val superAdder: Int => Int => Int =
    x => y => x + y

  val add3 = superAdder(3) // Int => Int = y => 3 + y
  println(add3(5))
  println(superAdder(3)(5))

  // METHOD!
  def curriedAdder(x: Int)(y: Int): Int = x + y // curried method

  val add4: Int => Int = curriedAdder(3)
  // won't compile without type declaration Int => Int unless use partial function apps
  // lifting = ETA-EXPANSION

  // functions != methods (JVM limitation)

  def inc(x: Int) = x + 1
  List(1,2,3).map(inc) // compiler does eta-expansion for us, turns inc into a func and uses func value on map
  // List(1,2,3).map(x => inc(x))

  // Partial function applications
  // lifting methods to functions
  val add5 = curriedAdder(5) _

  // Exercise
  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x + y
  def curriedAddMethod(x: Int)(y: Int) = x + y

  val add7 = (x: Int) => simpleAddFunction(7, x) // simplest
  val add7_2 = simpleAddFunction.curried(7)
  val add7_6 = simpleAddFunction(7, _: Int)

  val add7_3 = curriedAddMethod(7) _ // PAF
  val add7_4 = curriedAddMethod(7)(_) // PAF = alternative syntax

  val add7_5 = simpleAddMethod(7, _: Int) // alternative syntax for turning method into function values
  // rewritten as y => simpleAddMethod(7,y)

  // underscores are powerful
  def concatenator(a: String, b: String, c: String): String = a + b + c
  val insertName = concatenator("Hello, I'm ", _: String, ", how are you?")

  println(insertName("Lauren"))

  val fillInTheBlanks = concatenator("hello, ", _: String, _: String)
  // (x,y) => concatenator("hello, ", x, y)

  // exercises
  /*
    1. process a list of nums and return their string representations with diff formats
       Use the %4.2f, %8.6f, and %14.12f with a curried formatter function.
   */

  /*
    2. difference between
    - functions vs methods
    - parameters: by-name vs 0-lambda
   */

  def curriedFormatter(s: String)(number: Double): String = s.format(number)
  val numbers = List(Math.PI, Math.E, 1, 9.8, 1.3e-12)

  val simpleFormat = curriedFormatter("%4.2f") _ // lift
  val seriousFormat = curriedFormatter("%8.6f") _
  val preciseFormat = curriedFormatter("%14.12f") _

  println(numbers.map(simpleFormat))
  println(numbers.map(curriedFormatter("%4.2f"))) // don't need _, compiler does eta-expansion automatically

  def byName(n: => Int) = n + 1
  def byFunction(f: () => Int) = f() + 1

  def method: Int = 42
  def parenMethod(): Int = 42

  /*
    calling byName and byFunction
    - int
    - method
    - parenMethod
    - lambda
    - PAF
   */
  byName(23) // ok
  byName(method) // ok
  byName(parenMethod()) // ook
  byName(parenMethod) // ok but beware ==> byName(parenMethod())

  // byName(() => 42) not ok
  byName((()=>42)())
  // byName(parenMethod _) not ok

  // byFunction(45) not ok
  // byFunction(method) => not ok; method is eval to 42, parameter-less methods can't be passed to HOF
  byFunction(parenMethod) // compiler does eta expansion
  byFunction(() => 42)
  byFunction(parenMethod _) // also works
}
