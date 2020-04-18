package server

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.IncomingConnection
import akka.stream.scaladsl.Sink
import scala.util.Success
import scala.util.Failure
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.model.HttpMethods
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.ContentTypes
import scala.concurrent.Future
import akka.http.scaladsl.model.Uri
import akka.stream.scaladsl.Flow

object Server extends App {
  implicit val system = ActorSystem("lowlevelapi")
  implicit val materialiser = ActorMaterializer()

  import system.dispatcher

  // val serverSource = Http().bind("localhost", 8080)
  // val connSink = Sink.foreach[IncomingConnection] { conn =>
  //   println(s"Accepted incoming conn from: ${conn.remoteAddress}")
  // }

  // val materialisedValueFromFlow = serverSource.to(connSink).run()
  // // val materialisedValueFromSink = serverSource.runWith(connSink)

  // materialisedValueFromFlow.onComplete {
  //   case Success(value)     => println("Server binding successful.")
  //   case Failure(exception) => println(s"Server binding failed: $exception")
  // }

  val syncReqHandler: HttpRequest => HttpResponse = {
    case HttpRequest(HttpMethods.GET, _, _, _, _) =>
      HttpResponse(
        StatusCodes.OK,
        entity = HttpEntity(
          ContentTypes.`text/html(UTF-8)`,
          """
            |<!DOCTYPE html>
            |<html>
            | <body>
            |  <h1>Halko</h1>
            | </body>
            |</html> 
          """.stripMargin
        )
      )
    case request: HttpRequest =>
      request.discardEntityBytes()
      HttpResponse(
        StatusCodes.NotFound,
        entity = HttpEntity(
          ContentTypes.`text/html(UTF-8)`,
          """
            |<!DOCTYPE html>
            |<html>
            | <body>
            |  <h1>The resource can't be found</h1>
            | </body>
            |</html> 
          """.stripMargin
        )
      )
  }

  val asyncReqHandler: HttpRequest => Future[HttpResponse] = {
    case HttpRequest(HttpMethods.GET, Uri.Path("/home"), _, _, _) =>
      Future(
        HttpResponse(
          StatusCodes.OK,
          entity = HttpEntity(
            ContentTypes.`text/html(UTF-8)`,
            """
              |<h1>home</h1>
            """.stripMargin
          )
        )
      )
    case request: HttpRequest =>
      request.discardEntityBytes()
      Future(HttpResponse(StatusCodes.NotFound))
  }

  val streamBasedReqHandler: Flow[HttpRequest, HttpResponse, _] =
    Flow[HttpRequest].map {
      case HttpRequest(HttpMethods.GET, Uri.Path("/home"), _, _, _) =>
        HttpResponse(
          StatusCodes.OK,
          entity = HttpEntity(ContentTypes.`text/html(UTF-8)`, "<h1>home</h1>")
        )
    }

  val httpSyncConnectonHandler = Sink.foreach[IncomingConnection] { conn =>
    conn.handleWithSyncHandler(syncReqHandler)
  }

  Http().bind("localhost", 8080).runWith(httpSyncConnectonHandler)
  // Http().bindAndHandleSync(reqHandler, "localhost", 8080)
  Http().bindAndHandleAsync(asyncReqHandler, "localhost", 8081)
  Http().bindAndHandle(streamBasedReqHandler, "localhost", 8082)
}
