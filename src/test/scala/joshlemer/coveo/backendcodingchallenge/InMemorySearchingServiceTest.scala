package joshlemer.coveo.backendcodingchallenge

import joshlemer.coveo.backendcodingchallenge.Scorer.StringScorer
import org.scalatest.{FlatSpec, Matchers}

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

    searchingService.search(limitedQuery) should have length 5
  }
  it should "produce a lot of records when there's no limit" in {

    val limitedQuery = baseQuery.copy(limit = None)

    searchingService.search(limitedQuery) should have length totalRecords
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

//    val ls = StringScorer.longestSubstring("hellga", "hello")
//
//    val sq = SearchQuery(queryString = "hellga", latLong = None, limit = None)
//    val result2 = Scorer.getDefault.score(sq, similarCity)
//    val result3 = Scorer.getDefault.score(sq, dissimilarCity)

    val results =
      new InMemorySearchingService(citiesWhichDifferOnlyInName, Scorer.getDefault)
        .search(SearchQuery(queryString = "hellga", latLong = None, limit = Some(limit)))
        .map(_.city)

    results should contain theSameElementsInOrderAs expectedResults


  }

}
