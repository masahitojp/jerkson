package com.codahale.jerkson.tests

import scala.collection._
import com.codahale.jerkson.Json._
import org.scalatest.{Matchers, FreeSpec}

class CollectionSupportSpec extends FreeSpec with Matchers {
  "A collection.BitSet" - {
    "generates a JSON array of ints" - {
      generate(BitSet(1)) should be ("[1]")
    }

    "is parsable from a JSON array of ints" - {
      parse[BitSet]("[1,2,3]") should be (BitSet(1, 2, 3))
    }
  }

  "A collection.Iterator[Int]" - {
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

  "A collection.Traversable[Int]" - {
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

  "A collection.BufferedIterator[Int]" - {
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

  "A collection.Iterable[Int]" - {
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

  "A collection.Set[Int]" - {
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

  "A collection.Map[String, Int]" - {
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

  "A collection.IndexedSeq[Int]" - {
    "generates a JSON array of ints" - {
      generate(IndexedSeq(1, 2, 3)) should be ("[1,2,3]")
    }

    "is parsable from a JSON array of ints" - {
      parse[IndexedSeq[Int]]("[1,2,3]") should be (IndexedSeq(1, 2, 3))
    }

    "is parsable from an empty JSON array" - {
      parse[IndexedSeq[Int]]("[]") should be (IndexedSeq.empty)
    }
  }

  "A collection.Seq[Int]" - {
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

  "A collection.SortedMap[String, Int]" - {
    "generates a JSON object with int field values" - {
      generate(SortedMap("one" -> 1, "two" -> 2)) should be ("""{"one":1,"two":2}""")
    }

    // TODO: 6/1/11 <coda> -- figure out how to deserialize SortedMap instances

    /**
     * I think all this would take is a mapping from Class[_] to Ordering, which
     * would need to have hard-coded the various primitive types, and then add
     * support for Ordered and Comparable classes. Once we have the Ordering,
     * we can pass it in manually to a builder.
     */

    "is parsable from a JSON object with int field values" ignore {
      parse[SortedMap[String, Int]]("""{"one":1,"two":2}""") should be (SortedMap("one" -> 1, "two" -> 2))
    }

    "is parsable from an empty JSON object" ignore {
      parse[SortedMap[String, Int]]("{}") should be (SortedMap.empty[String, Int])
    }
  }

  "A collection.SortedSet[Int]" - {
    "generates a JSON array of ints" - {
      generate(SortedSet(1, 2, 3)) should be ("[1,2,3]")
    }

    // TODO: 6/1/11 <coda> -- figure out how to deserialize SortedMap instances

    /**
     * I think all this would take is a mapping from Class[_] to Ordering, which
     * would need to have hard-coded the various primitive types, and then add
     * support for Ordered and Comparable classes. Once we have the Ordering,
     * we can pass it in manually to a builder.
     */

    "is parsable from a JSON array of ints" ignore {
      parse[SortedSet[Int]]("[1,2,3]") should be (SortedSet(1, 2, 3))

    }

    "is parsable from an empty JSON array" ignore {
      parse[SortedSet[Int]]("[]") should be (SortedSet.empty[Int])
    }
  }
}
