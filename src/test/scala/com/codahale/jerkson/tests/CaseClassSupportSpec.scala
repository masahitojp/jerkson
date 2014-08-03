package com.codahale.jerkson.tests

import com.codahale.jerkson.Json._

import com.codahale.jerkson.ParsingException
import com.fasterxml.jackson.databind.node.IntNode
import org.scalatest.{Matchers, FreeSpec}

class CaseClassSupportSpec extends FreeSpec with Matchers {
  "A basic case class" - {
    "generates a JSON object with matching field values" - {
      generate(CaseClass(1, "Coda")) should be ("""{"id":1,"name":"Coda"}""")
    }

    "is parsable from a JSON object with corresponding fields" - {
      parse[CaseClass]("""{"id":1,"name":"Coda"}""") should be (CaseClass(1, "Coda"))
    }

    "is parsable from a JSON object with extra fields" - {
      parse[CaseClass]("""{"id":1,"name":"Coda","derp":100}""") should be (CaseClass(1, "Coda"))
    }

    "is not parsable from an incomplete JSON object" - {
      val thrown = intercept[ParsingException] {
        parse[CaseClass]("""{"id":1}""")
      }
      thrown.getMessage should be ("""Invalid JSON. Needed [id, name], but found [id].""")
    }
  }

  "A case class with lazy fields" - {
    "generates a JSON object with those fields evaluated" - {
      generate(CaseClassWithLazyVal(1)) should be ("""{"id":1,"woo":"yeah"}""")
    }

    "is parsable from a JSON object without those fields" - {
      parse[CaseClassWithLazyVal]("""{"id":1}""") should be (CaseClassWithLazyVal(1))
    }

    "is not parsable from an incomplete JSON object" - {
      val thrown = intercept[ParsingException] {
        parse[CaseClassWithLazyVal]("""{}""")
      }
      thrown.getMessage should be ("""Invalid JSON. Needed [id], but found [].""")
    }
  }

  "A case class with ignored members" - {
    "generates a JSON object without those fields" - {
      generate(CaseClassWithIgnoredField(1)) should be ("""{"id":1}""")
      generate(CaseClassWithIgnoredFields(1)) should be ("""{"id":1}""")
    }

    "is parsable from a JSON object without those fields" - {
      parse[CaseClassWithIgnoredField]("""{"id":1}""") should be (CaseClassWithIgnoredField(1))
      parse[CaseClassWithIgnoredFields]("""{"id":1}""") should be (CaseClassWithIgnoredFields(1))
    }

    "is not parsable from an incomplete JSON object" - {
      val thrownA = intercept[ParsingException] {
        parse[CaseClassWithIgnoredField]("""{}""")
      }
      thrownA.getMessage should be ("""Invalid JSON. Needed [id], but found [].""")

      val thrownB = intercept[ParsingException] {
        parse[CaseClassWithIgnoredFields]("""{}""")
      }
      thrownB.getMessage should be ("""Invalid JSON. Needed [id], but found [].""")
    }
  }

  "A case class with transient members" - {
    "generates a JSON object without those fields" - {
      generate(CaseClassWithTransientField(1)) should be ("""{"id":1}""")
    }

    "is parsable from a JSON object without those fields" - {
      parse[CaseClassWithTransientField]("""{"id":1}""") should be (CaseClassWithTransientField(1))
    }

    "is not parsable from an incomplete JSON object" - {
      val thrown = intercept[ParsingException] {
        parse[CaseClassWithTransientField]("""{}""")
      }
      thrown.getMessage should be ("""Invalid JSON. Needed [id], but found [].""")
    }
  }

  "A case class with an overloaded field" - {
    "generates a JSON object with the nullary version of that field" - {
      generate(CaseClassWithOverloadedField(1)) should be ("""{"id":1}""")
    }
  }

  "A case class with an Option[String] member" - {
    "generates a field if the member is Some" - {
      generate(CaseClassWithOption(Some("what"))) should be ("""{"value":"what"}""")
    }

    "is parsable from a JSON object with that field" - {
      parse[CaseClassWithOption]("""{"value":"what"}""") should be (CaseClassWithOption(Some("what")))
    }

    "doesn't generate a field if the member is None" - {
      generate(CaseClassWithOption(None)) should be ("""{}""")
    }

    "is parsable from a JSON object without that field" - {
      parse[CaseClassWithOption]("""{}""") should be (CaseClassWithOption(None))
    }

    "is parsable from a JSON object with a null value for that field" - {
      parse[CaseClassWithOption]("""{"value":null}""") should be (CaseClassWithOption(None))
    }
  }

  "A case class with a JsonNode member" - {
    "generates a field of the given type" - {
      generate(CaseClassWithJsonNode(new IntNode(2))) should be ("""{"value":2}""")
    }
  }

  "A case class with members of all ScalaSig types" - {
    val json = """
               {
                 "map": {
                   "one": "two"
                 },
                 "set": [1, 2, 3],
                 "string": "woo",
                 "list": [4, 5, 6],
                 "seq": [7, 8, 9],
                 "sequence": [10, 11, 12],
                 "collection": [13, 14, 15],
                 "indexedSeq": [16, 17, 18],
                 "randomAccessSeq": [19, 20, 21],
                 "vector": [22, 23, 24],
                 "bigDecimal": 12.0,
                 "bigInt": 13,
                 "int": 1,
                 "long": 2,
                 "char": "x",
                 "bool": false,
                 "short": 14,
                 "byte": 15,
                 "float": 34.5,
                 "double": 44.9,
                 "any": true,
                 "anyRef": "wah",
                 "intMap": {
                   "1": "1"
                 },
                 "longMap": {
                   "2": 2
                 }
               }
               """


    "is parsable from a JSON object with those fields" - {
      parse[CaseClassWithAllTypes](json) should be (
        CaseClassWithAllTypes(
          map = Map("one" -> "two"),
          set = Set(1, 2, 3),
          string = "woo",
          list = List(4, 5, 6),
          seq = Seq(7, 8, 9),
          indexedSeq = IndexedSeq(16, 17, 18),
          vector = Vector(22, 23, 24),
          bigDecimal = BigDecimal("12.0"),
          bigInt = BigInt("13"),
          int = 1,
          long = 2L,
          char = 'x',
          bool = false,
          short = 14,
          byte = 15,
          float = 34.5f,
          double = 44.9d,
          any = true,
          anyRef = "wah",
          intMap = Map(1 -> 1),
          longMap = Map(2L -> 2L)
        )
      )
    }
  }

  "A case class nested inside of an object" - {
    "is parsable from a JSON object" - {
      parse[OuterObject.NestedCaseClass]("""{"id": 1}""") should be (OuterObject.NestedCaseClass(1))
    }
  }

  "A case class nested inside of an object nested inside of an object" - {
    "is parsable from a JSON object" - {
      parse[OuterObject.InnerObject.SuperNestedCaseClass]("""{"id": 1}""") should be (OuterObject.InnerObject.SuperNestedCaseClass(1))
    }
  }

  "A case class with two constructors" - {
    "is parsable from a JSON object with the same parameters as the case accessor" - {
      parse[CaseClassWithTwoConstructors]("""{"id":1,"name":"Bert"}""") should be (CaseClassWithTwoConstructors(1, "Bert"))
    }

    "is parsable from a JSON object which works with the second constructor" - {
      intercept[ParsingException] {
        parse[CaseClassWithTwoConstructors]("""{"id":1}""")
      }
    }
  }

  "A case class with snake-cased fields" - {
    "is parsable from a snake-cased JSON object" - {
      parse[CaseClassWithSnakeCase]("""{"one_thing":"yes","two_thing":"good"}""") should be (CaseClassWithSnakeCase("yes", "good"))
    }

    "generates a snake-cased JSON object" - {
      generate(CaseClassWithSnakeCase("yes", "good")) should be ("""{"one_thing":"yes","two_thing":"good"}""")
    }

    "throws errors with the snake-cased field names present" - {
      val thrown = intercept[ParsingException] {
        parse[CaseClassWithSnakeCase]("""{"one_thing":"yes"}""")
      }
      thrown.getMessage should be ("""Invalid JSON. Needed [one_thing, two_thing], but found [one_thing].""")
    }
  }

  "A case class with array members" - {
    "is parsable from a JSON object" - {
      val c = parse[CaseClassWithArrays]("""{"one":"1","two":["a","b","c"],"three":[1,2,3]}""")

      c.one should be ("1")
      c.two should be (Array("a", "b", "c"))
      c.three should be (Array(1, 2, 3))
    }

    "generates a JSON object" - {
      generate(CaseClassWithArrays("1", Array("a", "b", "c"), Array(1, 2, 3))) should be (
        """{"one":"1","two":["a","b","c"],"three":[1,2,3]}"""
      )
    }
  }
}
