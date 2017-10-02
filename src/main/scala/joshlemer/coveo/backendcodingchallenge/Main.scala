package joshlemer.coveo.backendcodingchallenge

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import joshlemer.coveo.backendcodingchallenge.parsing.Geoname

object Main {
  def main(args: Array[String]): Unit = {

    implicit val searchingService: SearchingService[Id] =
      new InMemorySearchingService(
        cities = Geoname.loadCitiesInMemory("data/cities_canada-usa.tsv"),
        scorer = Scorer.getDefault
      )

    implicit val conf = Conf.defaults
    implicit val system = ActorSystem(conf.appName)
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher
    implicit val api: Api = new ApiImpl

    (new Services).start()
  }
}

object TestRun {
  def main(args: Array[String]): Unit = {
    implicit val searchingService: SearchingService[Id] =
      new InMemorySearchingService(
        cities = Geoname.loadCitiesInMemory("data/cities_canada-usa.tsv"),
        scorer = Scorer.getDefault
      )

    searchingService
      .search(SearchQuery(queryString = "Abbotsford", latLong = Some(LatLong(55.0, 73.2)), limit = Some(10)))
      .foreach(println)
  }
}
