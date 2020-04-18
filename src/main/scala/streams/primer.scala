package primer

import akka.actor.typed.{ActorSystem, ActorRef, Behavior}
import akka.stream.scaladsl.{Sink, Source}
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.scaladsl.Flow
import scala.concurrent.Future
import akka.stream.scaladsl.Keep
import akka.stream.OverflowStrategy
import scala.util.Success
import scala.util.Failure

object Hello {
  final case class Greet(whom: String, replyTo: ActorRef[Greeted])
  final case class Greeted(whom: String, from: ActorRef[Greet])

  def apply(): Behavior[Greet] = Behaviors.receive { (ctx, msg) =>
    ctx.log.info("Hello {}!", msg.whom)
    msg.replyTo ! Greeted(msg.whom, ctx.self)
    Behaviors.same
  }
}

object Primer extends App {
  implicit val system = ActorSystem(Hello(), "efe")

  val source = Source(1 to 10)
  val sink = Sink.foreach[Int](println)

  val graph = source.to(sink)
  graph.run()

  val flow = Flow[Int].map(_ + 1)
  val sourcePrim = source.via(flow)
  val sinkPrim = flow.to(sink)

  // sources
  val single = Source.single(1)
  val empty = Source.empty[Int]
  val infinite = Source(Stream.from(1))
  import scala.concurrent.ExecutionContext.Implicits.global
  val future = Source.fromFuture(Future(42))

  // sinks
  Sink.ignore
  Sink.head[Int] // takes first, close the stream
  val doneSink = Sink.foreach[Int](println)
  Sink.fold[Int, Int](0)((a, b) => a + b)

  // flows
  Flow[Int].take(5)
  // no flatMap (!)

  // compositions
  // runs on the same actor (single CPU core) -> operator fusion
  Source(1 to 10).map(x => x + 1).runForeach(println)
  Source(1 to 10)
    .via(Flow[Int].map(x => x + 1))
    .to(Sink.foreach[Int](println))
  // .run()

  // meterializations

  // materializations -> running the graph
  // materialized value -> obtained by runing the graph,
  // it could be anything and have nothing to do with the elements flowing through the stream(s)
  // materializing the graph -> materializing all of its components

  val graphFuture = Source(1 to 10)
    .viaMat(Flow[Int].map(x => x + 1))(Keep.right)
    .toMat(doneSink)(Keep.right)
  // .run()

  val sumSink = Sink.fold[Int, Int](0)((curr, elem) => curr + elem)
  val materializedSum = source runWith sumSink
  materializedSum onComplete {
    case Success(value)     => println(s"Sum $value")
    case Failure(exception) => println(s"Exception: $exception")
  }
  val flowMaterialisedValue =
    source.viaMat(flow)(Keep.right).toMat(sink)(Keep.left).run()

  // async boundries - parallelization

  // better throughput for time consuming operations
  // should be avoided when operationsn are comparable with a message pass

  // order of consnuption of emitted values is quaranteed, order of flow's evaluation is not quaranteed
  Source(1 to 10)
    .via(Flow[Int].map(x => {
      Thread.sleep(1000)
      x + 1
    }))
    .async // 1st actor
    .via(Flow[Int].map(x => {
      Thread.sleep(1000)
      x * 2
    }))
    .async // 2nd actor
    .to(doneSink) // 3rd actor
  // .run()

  // backpressure
  // fast producer, slow consumer -> consumer wil send a signal upstream for a consumer to slow down,
  // first 16 elements are buffered by default
  Source(1 to 1000).async
    .via(
      Flow[Int]
        .map(_ + 1)
        .buffer(10, overflowStrategy = OverflowStrategy.dropHead)
    )
    .async
    .to(Sink.foreach[Int] { _ =>
      Thread.sleep(1000)
    })
  // .run

  // manually triggering back pressure - throttling

}
