package joshlemer.coveo.backendcodingchallenge

import scala.language.higherKinds
import com.rockymadden.stringmetric.StringMetric
import com.rockymadden.stringmetric.similarity.JaroMetric

case class Score private (toDouble: Double) extends AnyVal

object Score {

  def from(double: Double): Option[Score] = if(0d <= double && double <= 1.0d) Some(Score(double)) else None

  /** Reduces the score to 0 or 1 if out of bounds */
  def trim(double: Double): Score = Score {
    if (double < 0d) 0d else if (double > 1d) 1d else double
  }

  def weightedAverage(score0: (Score, Double), scores: (Score, Double)*): Score = {
    require(((score0._2.toDouble +: scores.map(_._2.toDouble)).sum - 1.0).abs < 0.001,
      "Sum of weights must be about 1.0")

    Score.trim(
      (score0 +: scores).foldLeft(0d){ case (acc, (score, w)) => acc + (score.toDouble * w) }
    )
  }
}

trait Scorer[-T] { self =>
  def score(elem: T): Score

  def contramap[TT](f: TT => T): Scorer[TT] = new Scorer[TT] {
    def score(elem: TT): Score = self.score(f(elem))
  }
}

object Scorer {

  class WeightedAverageScorer[T](scorers: (Scorer[T], Double)*) extends Scorer[T] {
    require((scorers.map(_._2).sum - 1.0).abs < 0.001, "Sum of weights must be about 1.0")

    def score(elem: T): Score = Score.trim(
      scorers.foldLeft(0D) { case (acc, (scorer, weight)) => acc + (scorer.score(elem).toDouble * weight) }
    )
  }

  object Distance {

    /** Half the circumference of the Earth, the longest distance possible between two points */
    def HalfEarthsDiameterInMeters: Double = LatLong(0D, 0D) distanceInMeters LatLong(0D, 179.99D)

    /** Scores a 0 for antipodal cities, 1 for exactly precise cities, and linearly improves score along the way */
    class LinearScorer extends Scorer[(LatLong, LatLong)] {
      def score(elem: (LatLong, LatLong)): Score = Score.trim {
        val x = 1d - ((elem._1 distanceInMeters elem._2) / HalfEarthsDiameterInMeters)
        x
      }
    }
  }


  /** Via longerst common substring / total target string size */
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

trait SearchingService[M[+_]] {
  def search(searchQuery: SearchQuery): M[Seq[SearchResult]]
}

class InMemorySearchingService(cities: Seq[City], scorer: Scorer[(SearchQuery, City)])
  extends SearchingService[Id] {

  def search(searchQuery: SearchQuery): Seq[SearchResult] =
    cities.view
      .map(c => SearchResult(c, scorer.score((searchQuery, c))))
      .sortBy(_.score.toDouble)(implicitly[Ordering[Double]].reverse)
      .|>(results => searchQuery.limit.map(lim => results.take(lim)).getOrElse(results))
      .toList
}
