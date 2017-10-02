package joshlemer.coveo.backendcodingchallenge

import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeq
import scala.util.Random

class InMemorySearchingServiceTest extends FlatSpec with Matchers {

  behavior of "search()"

  val totalRecords = 1000

  val cities: IndexedSeq[City] = (1 to totalRecords).map(i =>
    City(name = i.toString, latLong = LatLong(Random.nextDouble() * 180 - 90, Random.nextDouble() * 180))
  )

  val searchingService = new InMemorySearchingService(cities, Scorer.getDefault)

  val baseQuery = SearchQuery("name", Some(LatLong(4D, 5D)), Some(10))

  it should "respect limits" in {

    val limit = 5

    val limitedQuery = baseQuery.copy(limit = Some(limit))

    searchingService.search(limitedQuery).searchResults should have length 5
  }
  it should "produce a lot of records when there's no limit" in {

    val limitedQuery = baseQuery.copy(limit = None)

    searchingService.search(limitedQuery).searchResults should have length totalRecords
  }
  it should "favor similar strings over non similar strings" in {

    val dissimilarCity = City("the name", LatLong(1D, 2D))

    val similarCity = City("hello", LatLong(1D, 2D))
    val similarCities = 4

    val citiesWhichDifferOnlyInName =
      (1 to 1000).map(_ => dissimilarCity) ++ (1 to similarCities).map(i => similarCity)

    val limit = 25

    val expectedResults =
      (1 to similarCities).map(_ => similarCity) ++
        (1 to (limit - similarCities)).map(_ => dissimilarCity)

    val results =
      new InMemorySearchingService(citiesWhichDifferOnlyInName, Scorer.getDefault)
        .search(SearchQuery(queryString = "hellga", latLong = None, limit = Some(limit)))
        .searchResults
        .map(_.city)

    results should contain theSameElementsInOrderAs expectedResults
  }
  it should "show results in decreasing score" in {

    def isMonotonicDescending(seq: Seq[Double]): Boolean = {
      @tailrec
      def inner(seq2: Seq[Double], prev: Option[Double]): Boolean =
        (seq2, prev) match {
          case (empty, _) if empty.isEmpty => true
          case (nonEmpty, Some(p)) if nonEmpty.head > p => false
          case (nonEmpty, _) => inner(nonEmpty.tail, Some(nonEmpty.head))
        }
      inner(seq, None)
    }

    isMonotonicDescending(searchingService.search(baseQuery.copy(limit = None))
      .searchResults
      .map(_.score.toDouble)) should be (true)

    isMonotonicDescending(searchingService.search(baseQuery.copy(limit = Some(20)))
      .searchResults
      .map(_.score.toDouble)) should be (true)

    isMonotonicDescending(searchingService.search(
      baseQuery.copy(latLong = Some(LatLong(1.2, 3.4)), limit = Some(20)))
      .searchResults
      .map(_.score.toDouble)) should be (true)
  }

}
