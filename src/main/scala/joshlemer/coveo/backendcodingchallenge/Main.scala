package joshlemer.coveo.backendcodingchallenge

import java.nio.charset.Charset

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.stream.ActorMaterializer
import joshlemer.coveo.backendcodingchallenge.parsing.Geoname

import scala.io.StdIn

object Main {


  def main(args: Array[String]): Unit = {
    val geonames = Geoname.parseFile("data/cities_canada-usa.tsv")

    val cities = geonames.map(_.toCity)

    implicit val searchingService: SearchingService[Id] =
      new InMemorySearchingService(cities.toSeq, Scorer.defaultScorer)

    searchingService
      .search(SearchQuery(queryString = "Abbotsford", latLong = Some(LatLong(55.0, 73.2)), limit = Some(10)))
      .foreach(println)

    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.dispatcher

    implicit val api: Api = new ApiImpl

    implicit val conf = Conf("localhost", 8080)

    (new Services).start()
  }
}
