package joshlemer.coveo.backendcodingchallenge

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directives, Route}
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import akka.stream.ActorMaterializer
import spray.json._

import scala.concurrent.ExecutionContext
import scala.io.StdIn
import scala.util.Try

trait Api {
  def route: Route
}


trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val searchResultJsonWriter: RootJsonFormat[SearchResult] = new RootJsonFormat[SearchResult] {
    override def write(obj: SearchResult): JsValue = JsObject(
      "name" -> JsString(obj.city.name),
      "latitude" -> JsString(obj.city.latLong.latitude.toString),
      "longitude" -> JsString(obj.city.latLong.longitude.toString),
      "score" -> JsString(obj.score.toDouble.toString)
    )
    def read(js: JsValue): SearchResult =
      SearchResult(
        city = City(
          name = fromField[String](js, "name"),
          latLong = LatLong(
            latitude = fromField[Double](js, "latitude"),
            longitude = fromField[Double](js, "longitude")
          )
        ),
        score = Score.trim(fromField[String](js, "score").toDouble)
      )
  }
}

object ApiImpl {
  def searchQueryFromParams(
    q: String,
    lat: Option[String],
    long: Option[String],
    lim: Option[String]): SearchQuery = {

    val latLong = for {
      la <- lat.flatMap(s => Try(s.toDouble).toOption)
      lo <- long.flatMap(s => Try(s.toDouble).toOption)
    } yield LatLong(la, lo)

    val limit = lim.flatMap(s => Try(s.toInt).toOption)

   SearchQuery(queryString = q, latLong = latLong, limit = limit)
  }
}

class ApiImpl(
  implicit system: ActorSystem,
  mat: ActorMaterializer,
  ec: ExecutionContext,
  searchingService: SearchingService[Id]) extends Api with Directives with JsonSupport {

  import ApiImpl._

  val route: Route =
    (path("suggestions") & get) {
      parameters('q, 'latitude.?, 'longitude.?, 'limit.?) { (q, lat, long, lim) =>
        complete(searchingService.search(searchQueryFromParams(q, lat, long, lim)))
      }
    }
}

case class Conf(interface: String, port: Int)

class Services(
  implicit api: Api,
  system: ActorSystem,
  mat: ActorMaterializer,
  ec: ExecutionContext,
  conf: Conf
) {

  def start(): Unit = {
    val bindingFuture = Http().bindAndHandle(api.route, conf.interface, conf.port)
    println(s"Server online at ${conf.interface}:${conf.port}/\nPress RETURN to stop...")
    StdIn.readLine()
    bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => system.terminate())
  }
}
