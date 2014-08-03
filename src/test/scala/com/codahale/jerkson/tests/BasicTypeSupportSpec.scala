package com.codahale.jerkson.tests

import org.scalatest.{Matchers, FreeSpec}
import com.codahale.jerkson.Json._
import com.fasterxml.jackson.databind.node.IntNode
import com.fasterxml.jackson.databind.JsonNode


class BasicTypeSupportSpec extends FreeSpec with Matchers {
  "A Byte`" - {
    "generates a JSON int" - {
      generate(15.toByte) should be("15")
    }

    "is parsable from a JSON int" - {
      parse[Byte]("15") should be(15)
    }
  }

  "A Short`" - {
    "generates a JSON int" - {
      generate(15.toShort) should be("15")
    }

    "is parsable from a JSON int" - {
      parse[Short]("15") should be(15)
    }
  }

  "An Int`" - {
    "generates a JSON int" - {
      generate(15) should be("15")
    }

    "is parsable from a JSON int" - {
      parse[Int]("15") should be(15)
    }
  }

  "A Long`" - {
    "generates a JSON int" - {
      generate(15L) should be("15")
    }

    "is parsable from a JSON int" - {
      parse[Long]("15") should be(15L)
    }
  }

  "A BigInt`" - {
    "generates a JSON int" - {
      generate(BigInt(15)) should be("15")
    }

    "is parsable from a JSON int" - {
      parse[BigInt]("15") should be(BigInt(15))
    }

    "is parsable from a JSON string" - {
      parse[BigInt]("\"15\"") should be(BigInt(15))
    }
  }

  "A Float`" - {
    "generates a JSON float" - {
      generate(15.1F) should be("15.1")
    }

    "is parsable from a JSON float" - {
      parse[Float]("15.1") should be(15.1F)
    }
  }

  "A Double`" - {
    "generates a JSON float" - {
      generate(15.1) should be("15.1")
    }

    "is parsable from a JSON float" - {
      parse[Double]("15.1") should be(15.1D)
    }
  }

  "A BigDecimal`" - {
    "generates a JSON float" - {
      generate(BigDecimal(15.5)) should be ("15.5")
    }

    "is parsable from a JSON float" - {
      parse[BigDecimal]("15.5") should be (BigDecimal(15.5))
    }

    "is parsable from a JSON int" - {
      parse[BigDecimal]("15") should be (BigDecimal(15.0))
    }
  }

  "A String`" - {
    "generates a JSON string" - {
      generate("woo") should be("\"woo\"")
    }

    "is parsable from a JSON string" - {
      parse[String]("\"woo\"") should be("woo")
    }
  }

  "A StringBuilder`" - {
    "generates a JSON string" - {
      generate(new StringBuilder("foo")) should be("\"foo\"")
    }

    "is parsable from a JSON string" - {
      parse[StringBuilder]("\"foo\"").toString() should be("foo")
    }
  }

  "A null Object`" - {
    "generates a JSON null" - {
      generate[Object](null) should be("null")
    }

    "is parsable from a JSON null" - {
      parse[Object]("null") should be (null)
    }
  }

  "A Boolean`" - {
    "generates a JSON true" - {
      generate(true) should be("true")
    }

    "generates a JSON false" - {
      generate(false)should be("false")
    }

    "is parsable from a JSON true" - {
      parse[Boolean]("true")should be(true)
    }

    "is parsable from a JSON false" - {
      parse[Boolean]("false")should be(false)
    }
  }

  "A Some[Int]`" - {
    "generates a JSON int" - {
      generate(Some(12))should be("12")
    }

    "is parsable from a JSON int as an Option[Int]" - {
      parse[Option[Int]]("12")should be(Some(12))
    }
  }

  "A None`" - {
    "generates a JSON null" - {
      generate(None)should be("null")
    }

    "is parsable from a JSON null as an Option[Int]" - {
      parse[Option[Int]]("null")should be(None)
    }
  }

  "A Left[String]`" - {
    "generates a JSON string" - {
      generate(Left("woo"))should be("\"woo\"")
    }

    "is parsable from a JSON string as an Either[String, Int]" - {
      parse[Either[String, Int]]("\"woo\"")should be(Left("woo"))
    }
  }

  "A Right[String]`" - {
    "generates a JSON string" - {
      generate(Right("woo"))should be("\"woo\"")
    }

    "is parsable from a JSON string as an Either[Int, String]" - {
      parse[Either[Int, String]]("\"woo\"")should be(Right("woo"))
    }
  }

  "A JsonNode`" - {
    "generates whatever the JsonNode is" - {
      generate(new IntNode(2))should be("2")
    }

    "is parsable from a JSON AST node" - {
      parse[JsonNode]("2")should be(new IntNode(2))
    }

    "is parsable from a JSON AST node as a specific type" - {
      parse[IntNode]("2")should be(new IntNode(2))
    }

    "is itself parsable" - {
      parse[Int](new IntNode(2))should be(2)
    }
  }

  "An Array[Int]`" - {
    "generates a JSON array of ints" - {
      generate(Array(1, 2, 3))should be("[1,2,3]")
    }

    "is parsable from a JSON array of ints" - {
      parse[Array[Int]]("[1,2,3]").toList should be(List(1, 2, 3))
    }

    "is parsable from an empty JSON array" - {
      parse[Array[Int]]("[]").toList should be(List.empty)
    }
  }
}
