package server

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.ContentTypes
import play.api.libs.json.Json
import scala.concurrent.duration._
import scala.util.Success
import scala.util.Failure

object HighLevelDSL extends App {

  object Routing {
    implicit val system = ActorSystem("high-level-dsl")
    implicit val materialiser = ActorMaterializer
    import system.dispatcher

    import akka.http.scaladsl.server.Directives._

    val simpleRoute = path("home") {
      get {
        complete(
          HttpEntity(
            ContentTypes.`application/json`,
            "{ \"kek\": true }"
          )
        )
      } ~
        post {
          complete(StatusCodes.OK)
        }
    }

    val extractionRoute = pathPrefix("api") {
      path("item" / IntNumber) { n =>
        println(s"got param: $n")
        complete(StatusCodes.OK)
      } ~ path("item") {
        // symbols are compared by reference eqality instead of contents equality
        parameter('id.as[Int]) { id =>
          println(s"got id: $id")
          complete(StatusCodes.OK)
        }
      }
    }

    val controlledEndpoint =
      (path("api" / "controlled") & get & extractRequest & extractLog) {
        (req, log) =>
          complete(StatusCodes.OK)
      }

    val dryRoute = ((path("about") | path("aboutUs"))) {
      complete(StatusCodes.OK)
    }

    // type Route = RequestContext => Future[RouteResult]
    val chainedRoute: Route = path("api" / "chained") {
      get {
        complete(StatusCodes.OK)
      } ~
        post {
          complete(StatusCodes.Forbidden)
        } ~ pathEndOrSingleSlash {
        complete(StatusCodes.ImATeapot)
      }
    } ~ simpleRoute ~ extractionRoute ~ controlledEndpoint ~ dryRoute

    ///
//   Http().bindAndHandle(chainedRoute, "localhost", 8080)
  }

  class Registry {

    case class Person(pin: Int, name: String)
    var people = List(
      Person(1, "Alice"),
      Person(2, "Bob"),
      Person(3, "Charlie")
    )

    implicit val personFormat = Json.format[Person]

    implicit val system = ActorSystem("registry")
    implicit val materialiser = ActorMaterializer
    import system.dispatcher

    import akka.http.scaladsl.server.Directives._

    lazy val peopleRoute = pathPrefix("api" / "people") {

      (get & ((parameter('pin.as[Int]) | path(IntNumber)))) { pin =>
        complete(
          HttpEntity(
            ContentTypes.`application/json`,
            people
              .find(x => x.pin == pin)
              .map(Json.toJson(_))
              .map(Json.stringify)
              .getOrElse("")
          )
        )
      } ~
        get {
          complete(
            HttpEntity(
              ContentTypes.`application/json`,
              Json.stringify(Json.toJson(people))
            )
          )
        } ~ (post & extractRequest & extractLog) { (req, log) =>
        val res = req.entity
          .toStrict(3 seconds)
          .map(_.data.utf8String)
          .map(Json.parse(_))
          .map(Json.fromJson(_).asOpt)

        onComplete(res) {
          case Success(Some(p)) => {
            log.info(s"Got person: $p")
            people = people :+ p
            complete(StatusCodes.OK)
          }
          case Failure(e) => failWith(e)
        }

      // res.onComplete {
      //   case Success(Some(p)) => {
      //     log.info(s"Got person: $p")
      //     people = people :+ p
      //   }
      //   case _ => log.warning("Sth wrong")
      // }

      // complete(res.map(_ => StatusCodes.OK).recover {
      //   case _ => StatusCodes.InternalServerError
      // })
      }
    }

    Http().bindAndHandle(peopleRoute, "localhost", 8081)
  }

  new Registry()
}
