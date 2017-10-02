package joshlemer.coveo.backendcodingchallenge

import org.scalatest.{FlatSpec, Matchers}

class ScoreTest extends FlatSpec with Matchers {

  behavior of "from"

  it should "produce a value only for in bound doubles" in {

    List(-1d, -0.5, -100, Double.MinValue, 1.1, 1.3, Double.MaxValue, 134.5, 4)
      .flatMap(Score.from) shouldBe empty

    List(0d, 0.0, 0.1, 0.4, 0.99999d, 1.0, 1.0000d)
      .flatMap(Score.from) should have length 7

  }

  behavior of "ScoreTest.trim"
  it should "trim" in {
    List(-1d, -0.5, -100, Double.MinValue, 1.1, 1.3, Double.MaxValue, 134.5, 4)
      .map(Score.trim) should be (List(0d, 0d, 0d, 0d, 1d, 1d, 1d, 1d, 1d).map(Score.trim))

    List(0d, 0.0, 0.1, 0.4, 0.99999d, 1.0, 1.0000d)
      .map(Score.trim) should be (List(0d, 0.0, 0.1, 0.4, 0.99999d, 1.0, 1.0000d).map(Score.trim))

  }
  behavior of "ScoreTest.fromWeightedAverage"

  it should "weightedAverage" in {
    Score.weightedAverage(
      Score.trim(0d) -> 0.20,
      Score.trim(1d) -> 0.20,
      Score.trim(0.5d) -> 0.20,
      Score.trim(1d) -> 0.20,
      Score.trim(0d) -> 0.20
    ) should be (Score.trim(0.5d))

  }

}
