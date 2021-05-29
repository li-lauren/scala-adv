package lectures.part3concurrency

import scala.collection.mutable
import scala.util.Random

object ThreadCommunication extends App {
  /*
    The producer-consumer problem

    producer -> [ x ] -> consumer
    running in parallel at the same time
    // need to force consumer to wait for producer
   */
  class SimpleContainer {
    private var value: Int = 0

    def isEmpty: Boolean = value == 0
    def set(newValue: Int) = value = newValue
    def get = {
      val result  = value
      value = 0
      result
    }
  }

  def naiveProdCons(): Unit = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting...")
      while(container.isEmpty) {
        println("[consumer] actively waiting")
      }

      println("[consumer] I have consumed " + container.get)
    })

    val producer = new Thread(() => {
      println("[producer] computing...")
      Thread.sleep(500)
      val value = 42
      println("[producer] I have produced, after long work, the value " + value)
      container.set(value)
    })

    consumer.start()
    producer.start()
  }

//  naiveProdCons()

  // wait and notify

  /**
   * Synchronized
   * Entering a synchrnoized expression on an object locks the object
   *
   * only AnyRefs can have synchronized blocks (primitive types like Int, Bool can't)
   *
   * General principles:
   * - make no assumptions about who gets the lock first
   * - keep locking to a minimum (for performance concerns)
   * - maintain thread safety at ALL times in parallel applications
   */

  /**
   * wait() and notify()
   * wait()-ing on an object's monitor suspends you (the thread) indefinitely
   * // thread 1
   * val someObject = "hello"
   * someObject.synchronized {
   * /// ... code part 1
   * someObject.wait()
   * /// ... code part 2
   * }
   * // thread 2
   * someObject.synchronized {
   * /// ... code
   * someObject.notify() <-- signal ONE sleepgin thread they may continue
   * (can't know which thread tho);
   * use notifyAll() to awaken ALL threads
   * /// ... more code
   */

  def smartProdCons(): Unit = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting....")
      container.synchronized {
        container.wait()
      }

      // container must have some value
      println("[consumer] I have consumed " + container.get)
    })

    val producer = new Thread(() => {
      println("[producer] Hard at work...")
      Thread.sleep(2000)
      val value = 42

      container.synchronized {
        println("[producer] I'm producing " + value)
        container.set(value)
        container.notify()
      }
    })

    consumer.start()
    producer.start()
  }

//  smartProdCons()

  /*
    producer => [ ? ? ? ] => consumer
    multiple values/can run indefinitely

    both producer and consumer may block each other
   */

  def prodConsLargeBuffer(): Unit = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]
    val capacity = 3

    val consumer = new Thread(() => {
      val random = new Random()

      while(true) {
        buffer.synchronized {
          if (buffer.isEmpty) {
            println("[consumer] buffer empty, waiting...")
            buffer.wait()
          }

          // there must be at least ONE value in the buffer
          val x = buffer.dequeue()
          println("[consumer] consumed " + x)

          // let's producer know there is empty space available
          buffer.notify()
        }

        Thread.sleep(random.nextInt(500))
      }
    })

    val producer = new Thread(() => {
      val random = new Random()
      var i = 0

      while(true) {
        buffer.synchronized {
          if(buffer.size == capacity) {
            println("[producer] buffer is full, waiting...")
            buffer.wait()
          }

          // there must be at least ONE empty space in the buffer
          println("[producer] producing " + i)
          buffer.enqueue(i)

          buffer.notify()

          i += 1
        }
        Thread.sleep(random.nextInt(500))
      }
    })

    consumer.start()
    producer.start()
  }

//  prodConsLargeBuffer()

  /*
    Prod-cons, level 3

    limited capacity buffer with multiple producers and multiple consumers

    producer1 -> [ ? ? ? ] -> consumer1
    producer2 ------^   ^---- consumer2
   */

  class Consumer(id: Int, buffer: mutable.Queue[Int]) extends Thread {
    override def run(): Unit = {
      val random = new Random()

      while(true) {
        buffer.synchronized {
          /*
            producer produces value, two Cons are waiting
            notifies ONE consumer, notifies on buffer
            notifies the other consumer
           */
          while (buffer.isEmpty) {
            println(s"[consumer $id] buffer empty, waiting...")
            buffer.wait()
          }

          // there must be at least ONE value in the buffer
          val x = buffer.dequeue()
          println(s"[consumer $id] consumed " + x)

          // signal producer/consumer to wake up
          buffer.notify()
        }

        Thread.sleep(random.nextInt(500))
      }
    }
  }

  class Producer(id: Int, buffer: mutable.Queue[Int], capacity: Int) extends Thread {
    override def run(): Unit = {
      val random = new Random()
      var i = 0

      while(true) {
        buffer.synchronized {
          while(buffer.size == capacity) {
            println(s"[producer $id] buffer is full, waiting...")
            buffer.wait()
          }

          // there must be at least ONE empty space in the buffer
          println(s"[producer $id] producing " + i)
          buffer.enqueue(i)

          buffer.notify()

          i += 1
        }
        Thread.sleep(random.nextInt(500))
      }
    }
  }

  def multiProdCons(nConsumers: Int, nProducers: Int): Unit = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]
    val capacity = 3

    (1 to nConsumers).foreach(i => new Consumer(i, buffer).start())
    (1 to nProducers).foreach(i => new Producer(i, buffer, capacity).start())
  }

//  multiProdCons(3,3)

  /*
    Exercises.
    1) Think of an example where notifyALL acts in a different way than notify?
    2) create a deadlock (where threads block each other)
    3) create a livelock (threads yield execution to each in such a way
    that nobody can continue)
   */

  // notifyAll
  def testNotifyAll(): Unit = {
    val bell = new Object

    (1 to 10).foreach(i => new Thread(() => {
      bell.synchronized {
        println(s"[thread $i] waiting ...")
        bell.wait()
        println(s"[thread $i] hooray!")
      }
    }).start())

    new Thread(() => {
      Thread.sleep(2000)
      println("[announcer] Rock 'n roll!")
      bell.synchronized {
        bell.notifyAll()
      }
    }).start()
  }

//  testNotifyAll()

  // 2 - deadlock
  case class Friend(name: String) {
    def bow(other: Friend) = {
      this.synchronized {
        println(s"$this: I am bowing to my friend $other")
        other.rise(this)
        println(s"$this: my friend $other has risen")
      }
    }

    def rise(other: Friend) = {
      this.synchronized {
        println(s"$this: I am rising to my friend $other")
      }
    }
    var side = "right"
    def switchSide(): Unit = {
      if (side == "right") side = "left"
      else side = "right"
    }
    def pass(other: Friend): Unit = {
      while(this.side == other.side) {
        println(s"$this: $other, feel free to pass")
        switchSide()
        Thread.sleep(1000)
      }
    }
  }

  val sam = Friend("Sam")
  val pierre = Friend("Pierre")

//  new Thread(() => sam.bow(pierre)).start() // sam's lock | pierre's lock
//  new Thread(() => pierre.bow(sam)).start() // pierre's lock | sam's lock

  // 3 - livelock
  new Thread(() => sam.pass(pierre)).start()
  new Thread(() => pierre.pass(sam)).start()
}
