package joshlemer.coveo.backendcodingchallenge

import scala.language.higherKinds

case class Score private (toDouble: Double) extends AnyVal

object Score {

  /** Returns The score if the double is in bounds, otherwise None */
  def from(double: Double): Option[Score] =
    if(0d <= double && double <= 1.0d) Some(Score(double)) else None

  /** Reduces the score to 0 or 1 if out of bounds */
  def trim(double: Double): Score = Score {
    if (double < 0d) 0d else if (double > 1d) 1d else double
  }

  /** Compute Score from taking the weighted average of many other scores */
  def weightedAverage(score0: (Score, Double), scores: (Score, Double)*): Score = {
    require(((score0._2.toDouble +: scores.map(_._2.toDouble)).sum - 1.0).abs < 0.001,
      "Sum of weights must be about 1.0")

    Score.trim(
      (score0 +: scores).foldLeft(0d){ case (acc, (score, w)) => acc + (score.toDouble * w) }
    )
  }
}

trait Scorer[-T] { self =>
  /** Evaluate an element */
  def score(elem: T): Score

  /** Create a scorer that leverages this scorer, by transforming input to what this scorer takes */
  def contramap[TT](f: TT => T): Scorer[TT] = new Scorer[TT] {
    def score(elem: TT): Score = self.score(f(elem))
  }
}

object Scorer {

  object Distance {

    /** Half the circumference of the Earth, the longest distance possible between two points */
    def HalfEarthsDiameterInMeters: Double = LatLong(0D, 0D) distanceInMeters LatLong(0D, 179.99D)

    /** Scores a 0 for antipodal cities, 1 for exactly precise cities, and linearly improves score along the way */
    class LinearScorer extends Scorer[(LatLong, LatLong)] {
      def score(elem: (LatLong, LatLong)): Score = Score.trim {
        1d - ((elem._1 distanceInMeters elem._2) / HalfEarthsDiameterInMeters)
      }
    }
  }


  /** Scores by longest common substring divided by total target string size */
  case object StringScorer extends Scorer[(String, String)] {
    def longestSubstring(elem: (String, String)): String = {
      val (a, b) = elem
      def loop(bestLengths: Map[(Int, Int), Int], bestIndices: (Int, Int), i: Int, j: Int): String = {
        if (i > a.length) {
          val bestJ = bestIndices._2
          b.substring(bestJ - bestLengths(bestIndices), bestJ)
        } else {
          val currentLength = if (a(i-1) == b(j-1)) bestLengths(i-1, j-1) + 1 else 0
          loop(
            bestLengths + ((i, j) -> currentLength),
            if (currentLength > bestLengths(bestIndices)) (i, j) else bestIndices,
            if (j == b.length) i + 1 else i,
            if (j == b.length) 1 else j + 1)
        }
      }

      loop(Map.empty[(Int, Int), Int].withDefaultValue(0), (0, 0), 1, 1)
    }
    def score(elem: (String, String)): Score = Score.trim {
      longestSubstring(elem._1, elem._2).length / elem._1.length
    }
  }

  /** By default, use String Scorer, and if a targit lat/long is present, weight that
    * equally with the String Scorer
    */
  val defaultScorer: Scorer[(SearchQuery, City)] = {
    val stringScorer =
     StringScorer.contramap[(SearchQuery, City)] { case (sq, c) => (sq.queryString, c.name)}

    val distanceScorer =
      (new Distance.LinearScorer).contramap[(LatLong, City)] { case (ll, c) => (ll, c.latLong)}

    new Scorer[(SearchQuery, City)] {
      def score(elem: (SearchQuery, City)): Score = {
        val (sq, city) = elem
        val distanceComponent = elem._1.latLong.map(ll => distanceScorer.score((ll, city)))
        val stringComponent = stringScorer.score(sq, city)

        distanceComponent
          .map(dist => Score.weightedAverage(dist -> 0.5, stringComponent -> 0.5))
          .getOrElse(stringComponent)
      }
    }
  }
}


case class SearchResult(city: City, score: Score)

case class SearchQuery(queryString: String, latLong: Option[LatLong], limit: Option[Int])

/** Service Layer for performing Searches
  * @tparam M Higher kinded type to wrap results. Typically will be some kind of Monad like
  *           Future or Id
  */
trait SearchingService[M[+_]] {
  def search(searchQuery: SearchQuery): M[Seq[SearchResult]]
}

/** Computes results from in-memory lookup of data, returns results synchronously through an `Id` */
class InMemorySearchingService(cities: Seq[City], scorer: Scorer[(SearchQuery, City)])
  extends SearchingService[Id] {

  def search(searchQuery: SearchQuery): Seq[SearchResult] =
    cities.view
      .map(c => SearchResult(c, scorer.score((searchQuery, c))))
      .sortBy(_.score.toDouble)(implicitly[Ordering[Double]].reverse)
      .|>(results => searchQuery.limit.map(lim => results.take(lim)).getOrElse(results))
      .toList
}
