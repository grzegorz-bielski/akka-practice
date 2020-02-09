package playground

import akka.actor.{Actor, ActorSystem}
import akka.actor.Props

object Playground extends App {
  val actorSystem = ActorSystem("Hello")

  println(actorSystem.name)

  object WordCountActor {
    def props(name: String) = Props(new WordCountActor())
  }

  class WordCountActor extends Actor {
    private var totalWords = 0

    def receive: PartialFunction[Any, Unit] = {
      case message: String =>
        print("got it")
        totalWords += message.split(" ").length
      case msg => println("what")
    }
  }

  val wordCountActor = actorSystem.actorOf(Props[WordCountActor], "wordCounter")
  val wordCountActor2 =
    actorSystem.actorOf(Props(new WordCountActor), "wordCounter2")

  wordCountActor ! "halko"
}
