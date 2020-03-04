package actors

import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorRef
import akka.actor.ActorSystem

object ChildActors extends App {

  object WordCounterMaster {
    case class Initialize(n: Int)
    case class WordCountTask(id: Int, text: String)
    case class WordCountReply(id: Int, count: Int)
  }
  class WordCounterMaster extends Actor {
    import WordCounterMaster._

    override def receive: Receive = {
      case Initialize(n) =>
        val refs =
          for (i <- 1 to n)
            yield context.actorOf(Props[WordCounterWorker], s"wcw_$i")

        context.become(withChildren(refs, 0, 0, Map()))
    }

    def withChildren(
        childrenRefs: Seq[ActorRef],
        childIndex: Int,
        taskIndex: Int,
        reqMap: Map[Int, ActorRef]
    ): Receive = {
      case text: String =>
        childrenRefs(childIndex) ! WordCountTask(taskIndex, text)

        val orgSender = sender()
        val nextChildIndex = (childIndex + 1) % childrenRefs.length
        val nextTaskIndex = taskIndex + 1
        val nextReqMap = reqMap + (taskIndex -> orgSender)

        context.become(
          withChildren(childrenRefs, nextChildIndex, nextTaskIndex, nextReqMap)
        )
      case WordCountReply(id, count) =>
        val originalSender = reqMap(id)
        originalSender ! count

        val nextReqMap = reqMap - id

        context.become(
          withChildren(childrenRefs, childIndex, taskIndex, nextReqMap)
        )
    }
  }

  class WordCounterWorker extends Actor {
    import WordCounterMaster._

    override def receive: Receive = {
      case WordCountTask(id, text) =>
        sender() ! WordCountReply(id, text.split(" ").length)
    }
  }

  class WordCounterClient extends Actor {
    import WordCounterMaster._

    override def receive: Actor.Receive = {
      case "go" =>
        val master = context.actorOf(Props[WordCounterMaster], "master")

        master ! Initialize(3)
        List("halko", "aaa", "bbbb ss cc ddd", "ccccc dffff").foreach(text =>
          master ! text
        )
      case count: Int =>
        println(s"[count: $count]")
    }
  }

  val system = ActorSystem("counting_sys")
  val client = system.actorOf(Props[WordCounterClient], "client")
  client ! "go"
}
