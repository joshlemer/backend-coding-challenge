package joshlemer.coveo.backendcodingchallenge

import scala.language.higherKinds

case class Score private (toDouble: Double) extends AnyVal

object Score {
  def from(double: Double): Option[Score] = if(0d <= double && double <= 1.0d) Some(Score(double)) else None

  def unsafe(double: Double): Score = Score(double)
}

object Scoring {

}

case class SearchResult(city: City, score: Score)

case class SearchQuery(queryString: String, latLong: Option[LatLong], limit: Option[Int])

trait SearchingService[M[+_]] {
  def search(searchQuery: SearchQuery): M[List[SearchResult]]
}

class InMemorySearchingService(cities: Seq[City], score: (SearchQuery, City) => Score)
  extends SearchingService[Id] {

  def search(searchQuery: SearchQuery) =
    cities.view.filter(_.name contains searchQuery.queryString)
      .map(c => SearchResult(c, score(searchQuery, c)))
      .|>(results => searchQuery.limit.map(lim => results.take(lim)).getOrElse(results))
      .toList
}
