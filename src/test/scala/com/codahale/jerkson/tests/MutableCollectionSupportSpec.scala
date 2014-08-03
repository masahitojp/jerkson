package com.codahale.jerkson.tests

import com.codahale.jerkson.Json._
import scala.collection.mutable._
import com.codahale.jerkson.ParsingException
import org.scalatest.{FreeSpec, Matchers}

class MutableCollectionSupportSpec extends FreeSpec with Matchers {
  "A mutable.ResizableArray[Int]" - {
    "generates a JSON array of ints" - {
      generate(ResizableArray(1, 2, 3)) should be ("[1,2,3]")
    }

    "is parsable from a JSON array of ints" - {
      parse[ResizableArray[Int]]("[1,2,3]") should be (ResizableArray(1, 2, 3))
    }

    "is parsable from an empty JSON array" - {
      parse[ResizableArray[Int]]("[]") should be (ResizableArray.empty[Int])
    }
  }

  "A mutable.ArraySeq[Int]" - {
    "generates a JSON array of ints" - {
      generate(ArraySeq(1, 2, 3)) should be ("[1,2,3]")
    }


    "is parsable from a JSON array of ints" - {
      parse[ArraySeq[Int]]("[1,2,3]") should be (ArraySeq(1, 2, 3))
    }

    "is parsable from an empty JSON array" - {
      parse[ArraySeq[Int]]("[]") should be (ArraySeq.empty[Int])
    }
  }

  "A mutable.MutableList[Int]" - {
    val xs = new MutableList[Int]
    xs ++= List(1, 2, 3)

    "generates a JSON array" - {
      generate(xs) should be ("[1,2,3]")
    }

    "is parsable from a JSON array of ints" - {
      parse[MutableList[Int]]("[1,2,3]") should be (xs)
    }

    "is parsable from an empty JSON array" - {
      parse[MutableList[Int]]("[]") should be (new MutableList[Int]())
    }
  }

  "A mutable.Queue[Int]" - {
    "generates a JSON array" - {
      generate(Queue(1, 2, 3)) should be ("[1,2,3]")
    }

    "is parsable from a JSON array of ints" - {
      parse[Queue[Int]]("[1,2,3]") should be (Queue(1, 2, 3))
    }

    "is parsable from an empty JSON array" - {
      parse[Queue[Int]]("[]") should be (new Queue[Int]())
    }
  }

  "A mutable.ListBuffer[Int]" - {
    "generates a JSON array" - {
      generate(ListBuffer(1, 2, 3)) should be ("[1,2,3]")
    }

    "is parsable from a JSON array of ints" - {
      parse[ListBuffer[Int]]("[1,2,3]") should be (ListBuffer(1, 2, 3))
    }

    "is parsable from an empty JSON array" - {
      parse[ListBuffer[Int]]("[]") should be (ListBuffer.empty[Int])
    }
  }

  "A mutable.ArrayBuffer[Int]" - {
    "generates a JSON array" - {
      generate(ArrayBuffer(1, 2, 3)) should be ("[1,2,3]")
    }

    "is parsable from a JSON array of ints" - {
      parse[ArrayBuffer[Int]]("[1,2,3]") should be (ArrayBuffer(1, 2, 3))
    }

    "is parsable from an empty JSON array" - {
      parse[ArrayBuffer[Int]]("[]") should be (ArrayBuffer.empty[Int])
    }
  }

  "A mutable.BitSet" - {
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

  "A mutable.HashSet[Int]" - {
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

  "A mutable.LinkedHashSet[Int]" - {
    "generates a JSON array" - {
      generate(LinkedHashSet(1)) should be ("[1]")
    }

    "is parsable from a JSON array of ints" - {
      parse[LinkedHashSet[Int]]("[1,2,3]") should be (LinkedHashSet(1, 2, 3))
    }

    "is parsable from an empty JSON array" - {
      parse[LinkedHashSet[Int]]("[]") should be (LinkedHashSet.empty[Int])
    }
  }

  "A mutable.Map[String, Int]" - {
    "generates a JSON object" - {
      generate(Map("one" -> 1)) should be ("""{"one":1}""")
    }

    "is parsable from a JSON object with int field values" - {
      parse[Map[String, Int]]("""{"one":1}""") should be (Map("one" -> 1))
    }

    "is parsable from an empty JSON object" - {
      parse[Map[String, Int]]("{}") should be (Map.empty[String, Int])
    }
  }

  "A mutable.Map[String, Any]" - {
    "is not parsable from an empty JSON object in a JSON array" - {
      intercept[ParsingException] {
        parse[Map[String, Any]]("[{}]")
      }
    }
  }

  "A mutable.HashMap[String, Int]" - {
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

  "A mutable.LinkedHashMap[String, Int]" - {
    "generates a JSON object" - {
      generate(LinkedHashMap("one" -> 1)) should be ("""{"one":1}""")
    }

    "is parsable from a JSON object with int field values" - {
      parse[LinkedHashMap[String, Int]]("""{"one":1}""") should be (LinkedHashMap("one" -> 1))
    }

    "is parsable from an empty JSON object" - {
      parse[LinkedHashMap[String, Int]]("{}") should be (LinkedHashMap.empty[String, Int])
    }
  }
}
