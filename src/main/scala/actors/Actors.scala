package actors

import akka.actor.ActorSystem
import akka.actor.AbstractActor.Receive
import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef

object Actors extends App {
  val system = ActorSystem("acone")

  case class Msg(contents: String)
  case class ForwardMsg(contents: String, ref: ActorRef)

  class SimpleActor extends Actor {
    override def receive: Receive = {
      case "Hi!" => context.sender() ! "Hello, there"
      case mesagge: String =>
        println(s"[simple actor] $mesagge ${context.self}")
      case Msg(contents)             => println(s"got $contents")
      case SayHiTo(ref)              => (ref ! "Hi")(self) // implicit self
      case ForwardMsg(contents, ref) => ref forward contents
    }
  }

  val simepleActor = system.actorOf(Props[SimpleActor], "simpleActor")

  // msgs must by immutable, serializable
  // can be of any type
  simepleActor ! 42
  simepleActor ! Msg("hey")

  val alice = system.actorOf(Props[SimpleActor], "alice")
  val bob = system.actorOf(Props[SimpleActor], "bob")

  case class SayHiTo(ref: ActorRef)
  alice ! SayHiTo(bob)

  object Counter {
    case object Increment
    case object Decrement
    case object Print
  }

  class Counter extends Actor {
    private var state = 0

    override def receive: Receive = {
      case Counter.Increment => state += 1
      case Counter.Decrement => state -= 1
      case Counter.Print     => println(state)
    }

  }

  class CounterImmutable extends Actor {
    override def receive: Actor.Receive = countReceive(0)

    def countReceive(state: Int): Actor.Receive = {
      case Counter.Increment => context.become(countReceive(state + 1))
      case Counter.Decrement => context.become(countReceive(state - 1))
      case Counter.Print     => println(state)
    }
  }

  object Account {
    case class Deposit(amount: Int)
    case class Withdraw(amount: Int)
    case object Statement

    case class Success(state: Int)
    case object Failure
  }

  class Account(init: Int) extends Actor {
    private var state = init

    override def receive: Actor.Receive = {
      case Account.Deposit(amount) =>
        sender ! {
          val res = state += amount
          Account.Success(state)
        }
      case Account.Withdraw(amount) =>
        sender ! {
          if (amount > state) Account.Failure
          else {
            state -= amount
            Account.Success(state)
          }
        }
      case Account.Statement => sender ! Account.Success(state)
    }
  }

  object Person {
    case class Live(account: ActorRef)
  }

  class Person extends Actor {
    import Person._
    import Account._

    override def receive: Actor.Receive = {
      case Live(account) => {
        account ! Deposit(10000)
        account ! Withdraw(9000)
        account ! Withdraw(500)
        account ! Statement
      }
      case message => println(message.toString)
    }
  }

  val counter = system.actorOf(Props[Counter], "counterActor")

  counter ! Counter.Increment
  counter ! Counter.Increment
  counter ! Counter.Print

  val account = system.actorOf(Props(new Account(0)), "accountActor")
  val person = system.actorOf(Props[Person], "someone")

  person ! Person.Live(account)

  // actor acts as a data structure
  // actor has a message queue
  // but can't run by itself, needs a thread

  // akka has a thread pool shared with actors
  // - only 1 thread can operatre on actor at any time (no locks needed)
  // - processing messages is atomic
  // - messages are deliver at most once, no duplicates
  // - order of messages is maintained

}
