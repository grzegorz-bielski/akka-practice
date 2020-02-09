package actors

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props

object Changeable extends App {

  object Fussy {
    case object Accept
    case object Reject

    val Ok = "Ok"
    val Nope = "Nope"
  }
  class Fussy extends Actor {
    import Fussy._
    import Overseer._

    private var state = Ok

    override def receive: Actor.Receive = {
      case Order(One) => state = Nope
      case Order(Two) => state = Ok
      case Ask(msg) => {
        if (state == Ok) sender ! Accept
        else sender ! Reject
      }
    }
  }

  class StatelessFussy extends Actor {
    import Fussy._
    import Overseer._

    override def receive: Actor.Receive = okReceive

    def okReceive: Actor.Receive = {
      case Order(One) =>
        context.become(nopeReceive, true) // replace the receive handler (default), will be used for all future messages
      case Order(Two) =>
      case Ask(_)     => sender ! Accept
    }
    def nopeReceive: Actor.Receive = {
      case Order(One) =>
        context.become(okReceive, discardOld = false) // `discardOld` pushes handler on the stack, will pop it on another msg
      case Order(Two) => context.unbecome() // takes from stack

      case Ask(_) => sender ! Reject
    }
  }

  object Overseer {
    case class Start(ref: ActorRef)
    case class Order(msg: String)
    case class Ask(msg: String)

    val One = "One"
    val Two = "Two"
  }
  class Overseer extends Actor {
    import Overseer._
    import Fussy._

    override def receive: Actor.Receive = {
      case Start(ref) =>
        ref ! Order(One)
        ref ! Ask("was that ok?")
      case Accept => println("it was accepted")
      case Reject => println("it was rejected")
    }
  }

  val system = ActorSystem("somesystem")
  val fussy = system.actorOf(Props[Fussy])
  val overseer = system.actorOf(Props[Overseer])

  // overseer ! Overseer.Start(fussy)
  case class Vote(candidate: String)
  case object VoteStatusRequest
  case class VoteStatusReply(candidate: Option[String])

  class Citizen extends Actor {
    override def receive: Actor.Receive = {
      case Vote(c)           => context.become(voted(c))
      case VoteStatusRequest => sender ! VoteStatusReply(None)
    }

    def voted(candidate: String): Receive = {
      case VoteStatusRequest => sender ! VoteStatusReply(Some(candidate))
    }
  }

  case class AggregateVotes(citizens: Set[ActorRef])
  class VoteAggregator extends Actor {
    override def receive: Actor.Receive = awaitingCommand

    def awaitingCommand: Receive = {
      case AggregateVotes(citizens) =>
        citizens.foreach(citizenRef => citizenRef ! VoteStatusRequest)
        context.become(awaitingStatuses(citizens, Map()))
    }

    def awaitingStatuses(
        stillWaiting: Set[ActorRef],
        prevStats: Map[String, Int]
    ): Receive = {
      case VoteStatusReply(None) => sender() ! VoteStatusRequest
      case VoteStatusReply(Some(candidate)) =>
        val currentStillWaiting = stillWaiting - sender()
        val currentVotesOfCandidate = prevStats.getOrElse(candidate, 0)
        val currentStats = prevStats + (candidate -> (currentVotesOfCandidate + 1))
        if (currentStillWaiting.isEmpty) {
          println(s"[aggregator] poll stats: $currentStats")
        } else {
          context.become(awaitingStatuses(currentStillWaiting, currentStats))
        }
    }
  }

  val alice = system.actorOf(Props[Citizen])
  val bob = system.actorOf(Props[Citizen])
  val charlie = system.actorOf(Props[Citizen])
  val daniel = system.actorOf(Props[Citizen])

  alice ! Vote("Martin")
  bob ! Vote("Jonas")
  charlie ! Vote("Roland")
  daniel ! Vote("Roland")

  val voteAggregator = system.actorOf(Props[VoteAggregator])
  voteAggregator ! AggregateVotes(Set(alice, bob, charlie, daniel))

}
