package joshlemer.coveo.backendcodingchallenge.parsing


import java.nio.charset.Charset

import better.files.{File, UnicodeDecoder}
import joshlemer.coveo.backendcodingchallenge.City

import scala.util.Try

case class Geoname(
  id: Int,
  name: String,
  ascii: String,
  alt_name: String,
  lat: Double,
  long: Double,
  feat_class: Char,
  feat_code: String, // 10 chars
  country: String, // 2 chars
  cc2: String,
  admin1: String,
  admin2: String,
  admin3: String,
  admin4: String,
  population: Long,
  elevation: Option[Int],
  dem: String,
  tz: String,
  modified_at: String
) {
  def toCity: City = City(name, lat, long)
}

object Geoname {


  val separator = "\t"

  def parseFile(path: String, correctForElevation: Boolean = true): Iterator[Geoname] = {
    implicit val charset = new UnicodeDecoder(Charset.defaultCharset()).charset()
    val (header, body) = File(path).lines.splitAt(1)
    val arr = header.head.split(separator)

    val headers = {
      List(
        "id",
        "name",
        "ascii",
        "alt_name",
        "lat",
        "long",
        "feat_class",
        "feat_code",
        "country",
        "cc2",
        "admin1",
        "admin2",
        "admin3",
        "admin4",
        "population",
        "elevation",
        "dem",
        "tz",
        "modified_at"
      )
    }

    // noticed there's an extra empty space in the rows between population and elevation
    def addElevationSpacer(header: String, index: Int): Int =
      if(correctForElevation && index >= headers.indexOf("elevation")) index //+ 1
      else index


    // array for performance
    val indexes: Array[Int] =
      headers
        .zipWithIndex
        .map { case (h, ind) => addElevationSpacer(h, arr.indexOf(h)) }
        .toArray

    body.map(_.split(separator)).map( row =>
      Geoname(
        id = row(indexes(0)).toInt,
        name = row(indexes(1)),
        ascii = row(indexes(2)),
        alt_name = row(indexes(3)),
        lat = row(indexes(4)).toDouble,
        long = row(indexes(5)).toDouble,
        feat_class = row(indexes(6)).head,
        feat_code = row(indexes(7)),
        country = row(indexes(8)),
        cc2 = row(indexes(9)),
        admin1 = row(indexes(10)),
        admin2 = row(indexes(11)),
        admin3 = row(indexes(12)),
        admin4 = row(indexes(13)),
        population = row(indexes(14)).toLong,
        elevation = Try(row(indexes(15)).toInt).toOption,
        dem = row(indexes(16)),
        tz = row(indexes(17)),
        modified_at = row(indexes(18))
      )
    ).toIterator
  }
}



