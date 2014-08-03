package com.codahale.jerkson.tests

import com.codahale.jerkson.Json._
import com.codahale.jerkson.ParsingException
import org.scalatest.{Matchers, FreeSpec}

class DefaultCollectionSupportSpec extends FreeSpec with Matchers {
  "A Range" - {
    "generates a JSON object" - {
      generate(Range.inclusive(1, 4, 3)) should be ("""{"start":1,"end":4,"step":3,"inclusive":true}""")
    }

    "generates a JSON object without the inclusive field if it's exclusive" - {
      generate(Range(1, 4, 3)) should be ("""{"start":1,"end":4,"step":3}""")
    }

    "generates a JSON object without the step field if it's 1" - {
      generate(Range(1, 4)) should be ("""{"start":1,"end":4}""")
    }

    "is parsable from a JSON object" - {
      parse[Range]("""{"start":1,"end":4,"step":3,"inclusive":true}""") should be (Range.inclusive(1, 4, 3))
    }

    "is parsable from a JSON object without the inclusive field" - {
      parse[Range]("""{"start":1,"end":4,"step":3}""") should be (Range(1, 4, 3))
    }

    "is parsable from a JSON object without the step field" - {
      parse[Range]("""{"start":1,"end":4}""") should be (Range(1, 4))
    }

    "is not parsable from a JSON object without the required fields" - {
      intercept[ParsingException] {
        parse[Range]("""{"start":1}""")
      }.getMessage should be ("""Invalid JSON. Needed [start, end, <step>, <inclusive>], but found [start].""")
    }

  }

  "A Pair[Int]" - {
    "generates a two-element JSON array of ints" ignore {
      // TODO: 5/31/11 <coda> -- fix Pair serialization
      generate(Pair(1, 2)) should be ("[1,2]")
    }

    "is parsable from a two-element JSON array of ints" ignore {
      // TODO: 5/31/11 <coda> -- fix Pair deserialization
      parse[Pair[Int, Int]]("[1,2]") should be (Pair(1, 2))
    }
  }

  "A Triple[Int]" - {
    "generates a three-element JSON array of ints" ignore {
      // TODO: 5/31/11 <coda> -- fix Triple serialization
      generate(Triple(1, 2, 3)) should be ("[1,2,3]")
    }

    "is parsable from a three-element JSON array of ints" ignore {
      // TODO: 5/31/11 <coda> -- fix Triple deserialization
      parse[Triple[Int, Int, Int]]("[1,2,3]") should be (Triple(1, 2, 3))
    }
  }

  "A four-tuple" - {
    "generates a four-element JSON array" ignore {
      // TODO: 5/31/11 <coda> -- fix Tuple4 serialization
      generate((1, "2", 3, "4")) should be ("[1,\"2\",3,\"4\"]")
    }

    "is parsable from a three-element JSON array of ints" ignore {
      // TODO: 5/31/11 <coda> -- fix Tuple4 deserialization
      parse[(Int, String, Int, String)]("[1,\"2\",3,\"4\"]") should be ((1, "2", 3, "4"))
    }
  }

  // TODO: 6/1/11 <coda> -- add support for all Tuple1->TupleBillionty types

  "A Seq[Int]" - {
    "generates a JSON array of ints" - {
      generate(Seq(1, 2, 3)) should be ("[1,2,3]")
    }

    "is parsable from a JSON array of ints" - {
      parse[Seq[Int]]("[1,2,3]") should be (Seq(1, 2, 3))
    }

    "is parsable from an empty JSON array" - {
      parse[Seq[Int]]("[]") should be (Seq.empty[Int])
    }
  }

  "A List[Int]" - {
    "generates a JSON array of ints" - {
      generate(List(1, 2, 3)) should be ("[1,2,3]")
    }

    "is parsable from a JSON array of ints" - {
      parse[List[Int]]("[1,2,3]") should be (List(1, 2, 3))
    }

    "is parsable from an empty JSON array" - {
      parse[List[Int]]("[]") should be (List.empty[Int])
    }
  }

  "An IndexedSeq[Int]" - {
    "generates a JSON array of ints" - {
      generate(IndexedSeq(1, 2, 3)) should be ("[1,2,3]")
    }

    "is parsable from a JSON array of ints" - {
      parse[IndexedSeq[Int]]("[1,2,3]") should be (IndexedSeq(1, 2, 3))
    }

    "is parsable from an empty JSON array" - {
      parse[IndexedSeq[Int]]("[]") should be (IndexedSeq.empty[Int])
    }
  }

  "A Vector[Int]" - {
    "generates a JSON array of ints" - {
      generate(Vector(1, 2, 3)) should be ("[1,2,3]")
    }

    "is parsable from a JSON array of ints" - {
      parse[Vector[Int]]("[1,2,3]") should be (Vector(1, 2, 3))
    }

    "is parsable from an empty JSON array" - {
      parse[Vector[Int]]("[]") should be (Vector.empty[Int])
    }
  }

  "A Set[Int]" - {
    "generates a JSON array of ints" - {
      generate(Set(1, 2, 3)) should be ("[1,2,3]")
    }

    "is parsable from a JSON array of ints" - {
      parse[Set[Int]]("[1,2,3]") should be (Set(1, 2, 3))
    }

    "is parsable from an empty JSON array" - {
      parse[Set[Int]]("[]") should be (Set.empty[Int])
    }
  }

  "A Map[String, Int]" - {
    "generates a JSON object with int field values" - {
      generate(Map("one" -> 1, "two" -> 2)) should be ("""{"one":1,"two":2}""")
    }

    "is parsable from a JSON object with int field values" - {
      parse[Map[String, Int]]("""{"one":1,"two":2}""") should be (Map("one" -> 1, "two" -> 2))
    }

    "is parsable from an empty JSON object" - {
      parse[Map[String, Int]]("{}") should be (Map.empty[String, Int])
    }
  }

  "A Map[String, Any]" - {
    "generates a JSON object with mixed field values" - {
      generate(Map("one" -> 1, "two" -> "2")) should be ("""{"one":1,"two":"2"}""")
    }

    "is parsable from a JSON object with mixed field values" - {
      parse[Map[String, Any]]("""{"one":1,"two":"2"}""") should be (Map[String, Any]("one" -> 1, "two" -> "2"))
    }

    "is parsable from an empty JSON object" - {
      parse[Map[String, Any]]("{}") should be (Map.empty[String, Any])
    }
  }

  "A Stream[Int]" - {
    "generates a JSON array" - {
      generate(Stream(1, 2, 3)) should be ("[1,2,3]")
    }

    "is parsable from a JSON array of ints" - {
      parse[Stream[Int]]("[1,2,3]") should be (Stream(1, 2, 3))
    }

    "is parsable from an empty JSON array" - {
      parse[Stream[Int]]("[]") should be (Stream.empty[Int])
    }
  }

  "An Iterator[Int]" - {
    "generates a JSON array of ints" - {
      generate(Seq(1, 2, 3).iterator) should be ("[1,2,3]")
    }

    "is parsable from a JSON array of ints" - {
      parse[Iterator[Int]]("[1,2,3]").toList should be (List(1, 2, 3))
    }

    "is parsable from an empty JSON array" - {
      parse[Iterator[Int]]("[]").toList should be (List.empty[Int])
    }
  }

  "A Traversable[Int]" - {
    "generates a JSON array of ints" - {
      generate(Seq(1, 2, 3).toTraversable) should be ("[1,2,3]")
    }

    "is parsable from a JSON array of ints" - {
      parse[Traversable[Int]]("[1,2,3]").toList should be (List(1, 2, 3))
    }

    "is parsable from an empty JSON array" - {
      parse[Traversable[Int]]("[]").toList should be (List.empty[Int])
    }
  }

  "A BufferedIterator[Int]" - {
    "generates a JSON array of ints" - {
      generate(Seq(1, 2, 3).iterator.buffered) should be ("[1,2,3]")
    }

    "is parsable from a JSON array of ints" - {
      parse[BufferedIterator[Int]]("[1,2,3]").toList should be (List(1, 2, 3))
    }

    "is parsable from an empty JSON array" - {
      parse[BufferedIterator[Int]]("[]").toList should be (List.empty[Int])
    }
  }

  "An Iterable[Int]" - {
    "generates a JSON array of ints" - {
      generate(Seq(1, 2, 3).toIterable) should be ("[1,2,3]")
    }

    "is parsable from a JSON array of ints" - {
      parse[Iterable[Int]]("[1,2,3]").toList should be (List(1, 2, 3))
    }

    "is parsable from an empty JSON array" - {
      parse[Iterable[Int]]("[]").toList should be (List.empty[Int])
    }
  }
}
