package com.codahale.jerkson.tests

import com.codahale.jerkson.Json._
import scala.collection.immutable._
import com.codahale.jerkson.ParsingException
import org.scalatest.{ShouldMatchers, FreeSpec}

class ImmutableCollectionSupportSpec extends FreeSpec with ShouldMatchers {
  "An immutable.Seq[Int]" - {
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

  "An immutable.List[Int]" - {
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

  "An immutable.IndexedSeq[Int]" - {
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

  "An immutable.TreeSet[Int]" - {
    "generates a JSON array" - {
      generate(TreeSet(1)) should be ("[1]")
    }

    // TODO: 6/1/11 <coda> -- figure out how to deserialize TreeSet instances

    /**
     * I think all this would take is a mapping from Class[_] to Ordering, which
     * would need to have hard-coded the various primitive types, and then add
     * support for Ordered and Comparable classes. Once we have the Ordering,
     * we can pass it in manually to a builder.
     */
    
    "is parsable from a JSON array of ints" ignore {
      parse[TreeSet[Int]]("[1,2,3]") should be (TreeSet(1, 2, 3))
    }

    "is parsable from an empty JSON array" ignore {
      parse[TreeSet[Int]]("[]") should be (TreeSet.empty[Int])
    }
  }

  "An immutable.HashSet[Int]" - {
    "generates a JSON array" - {
      generate(HashSet(1)) should be ("[1]")
    }

    "is parsable from a JSON array of ints" - {
      parse[HashSet[Int]]("[1,2,3]") should be (HashSet(1, 2, 3))
    }

    "is parsable from an empty JSON array" - {
      parse[HashSet[Int]]("[]") should be (HashSet.empty[Int])
    }
  }

  "An immutable.BitSet" - {
    "generates a JSON array" - {
      generate(BitSet(1)) should be ("[1]")
    }

    "is parsable from a JSON array of ints" - {
      parse[BitSet]("[1,2,3]") should be (BitSet(1, 2, 3))
    }

    "is parsable from an empty JSON array" - {
      parse[BitSet]("[]") should be (BitSet.empty)
    }
  }

  "An immutable.TreeMap[String, Int]" - {
    "generates a JSON object" - {
      generate(TreeMap("one" -> 1)) should be ("""{"one":1}""")
    }

    // TODO: 6/1/11 <coda> -- figure out how to deserialize TreeMap instances

    /**
     * I think all this would take is a mapping from Class[_] to Ordering, which
     * would need to have hard-coded the various primitive types, and then add
     * support for Ordered and Comparable classes. Once we have the Ordering,
     * we can pass it in manually to a builder.
     */
    
    "is parsable from a JSON object with int field values" ignore {
      parse[TreeMap[String, Int]]("""{"one":1}""") should be (TreeMap("one" -> 1))
    }

    "is parsable from an empty JSON object" ignore {
      parse[TreeMap[String, Int]]("{}") should be (TreeMap.empty[String, Int])
    }
  }

  "An immutable.HashMap[String, Int]" - {
    "generates a JSON object" - {
      generate(HashMap("one" -> 1)) should be ("""{"one":1}""")
    }

    "is parsable from a JSON object with int field values" - {
      parse[HashMap[String, Int]]("""{"one":1}""") should be (HashMap("one" -> 1))
    }

    "is parsable from an empty JSON object" - {
      parse[HashMap[String, Int]]("{}") should be (HashMap.empty[String, Int])
    }
  }

  "An immutable.HashMap[String, Any]" - {
    "generates a JSON object" - {
      generate(HashMap[String, Any]("one" -> 1)) should be ("""{"one":1}""")
    }

    "is parsable from a JSON object with int field values" - {
      parse[HashMap[String, Any]]("""{"one":1}""") should be (HashMap("one" -> 1))
    }

    "is parsable from an empty JSON object" - {
      parse[HashMap[String, Any]]("{}") should be (HashMap.empty[String, Any])
    }

    "is not parsable from an empty JSON object in a JSON array" - {
      intercept[ParsingException] {
        parse[HashMap[String, Any]]("[{}]")
      }
    }
  }

  "An immutable.Map[Int, String]" - {
    "generates a JSON object" - {
      generate(Map(1 -> "one")) should be ("""{"1":"one"}""")
    }

    "is parsable from a JSON object with decimal field names and string field values" - {
      parse[Map[Int, String]]("""{"1":"one"}""") should be (Map(1 -> "one"))
    }

    "is not parsable from a JSON object with non-decimal field names" - {
      intercept[ParsingException] {
        parse[Map[Int, String]]("""{"one":"one"}""")
      }
    }

    "is parsable from an empty JSON object" - {
      parse[Map[Int, String]]("{}") should be (Map.empty[Int, String])
    }
  }

  "An immutable.Map[Int, Any]" - {
    "is not parsable from an empty JSON object in a JSON array" - {
      intercept[ParsingException] {
        parse[Map[Int, Any]]("[{}]")
      }
    }
  }

  "An immutable.IntMap[Any]" - {
    "is not parsable from an empty JSON object in a JSON array" - {
      intercept[ParsingException] {
        parse[IntMap[Any]]("[{}]")
      }
    }
  }

  "An immutable.LongMap[Any]" - {
    "is not parsable from an empty JSON object in a JSON array" - {
      intercept[ParsingException] {
        parse[LongMap[Any]]("[{}]")
      }
    }
  }

  "An immutable.Map[Long, Any]" - {
    "is not parsable from an empty JSON object in a JSON array" - {
      intercept[ParsingException] {
        parse[Map[Long, Any]]("[{}]")
      }
    }
  }

  "An immutable.Map[Long, String]" - {
    "generates a JSON object" - {
      generate(Map(1L -> "one")) should be ("""{"1":"one"}""")
    }

    "is parsable from a JSON object with decimal field names and string field values" - {
      parse[Map[Long, String]]("""{"1":"one"}""") should be (Map(1L -> "one"))
    }

    "is not parsable from a JSON object with non-decimal field names" - {
      intercept[ParsingException] {
        parse[Map[Long, String]]("""{"one":"one"}""")
      }
    }

    "is parsable from an empty JSON object" - {
      parse[Map[Long, String]]("{}") should be (Map.empty[Long, String])
    }
  }

  "An immutable.IntMap[String]" - {
    "generates a JSON object" - {
      generate(IntMap(1 -> "one")) should be ("""{"1":"one"}""")
    }

    "is parsable from a JSON object with decimal field names and string field values" - {
      parse[IntMap[String]]("""{"1":"one"}""") should be (IntMap(1 -> "one"))
    }

    "is not parsable from a JSON object with non-decimal field names" - {
      intercept[ParsingException] {
        parse[IntMap[String]]("""{"one":"one"}""")
      }
    }

    "is parsable from an empty JSON object" - {
      parse[IntMap[String]]("{}") should be (IntMap.empty[String])
    }
  }

  "An immutable.LongMap[String]" - {
    "generates a JSON object" - {
      generate(LongMap(1L -> "one")) should be ("""{"1":"one"}""")
    }

    "is parsable from a JSON object with int field names and string field values" - {
      parse[LongMap[String]]("""{"1":"one"}""") should be (LongMap(1L -> "one"))
    }

    "is not parsable from a JSON object with non-decimal field names" - {
      intercept[ParsingException] {
        parse[LongMap[String]]("""{"one":"one"}""")
      }
    }

    "is parsable from an empty JSON object" - {
      parse[LongMap[String]]("{}") should be (LongMap.empty)
    }
  }

  "An immutable.Queue[Int]" - {
    "generates a JSON array" - {
      generate(Queue(1, 2, 3)) should be ("[1,2,3]")
    }

    "is parsable from a JSON array of ints" - {
      parse[Queue[Int]]("[1,2,3]") should be (Queue(1, 2, 3))
    }

    "is parsable from an empty JSON array" - {
      parse[Queue[Int]]("[]") should be (Queue.empty)
    }
  }
}
