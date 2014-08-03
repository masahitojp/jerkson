package com.codahale.jerkson.tests

import com.codahale.jerkson.Json._
import com.codahale.jerkson.AST._
import org.scalatest.{Matchers, FreeSpec}

class ASTTypeSupportSpec extends FreeSpec with Matchers {
  "An AST.JInt" - {
    "generates a JSON int" - {
      generate(JInt(15)) should be ("15")
    }

    "is parsable from a JSON int" - {
      parse[JInt]("15") should be (JInt(15))
    }

    "is parsable from a JSON int as a JValue" - {
      parse[JValue]("15") should be (JInt(15))
    }
  }

  "An AST.JFloat" - {
    "generates a JSON int" - {
      generate(JFloat(15.1)) should be ("15.1")
    }

    "is parsable from a JSON float" - {
      parse[JFloat]("15.1") should be (JFloat(15.1))
    }

    "is parsable from a JSON float as a JValue" - {
      parse[JValue]("15.1") should be (JFloat(15.1))
    }
  }


  "An AST.JString" - {
    "generates a JSON string" - {
      generate(JString("woo")) should be ("\"woo\"")
    }

    "is parsable from a JSON string" - {
      parse[JString]("\"woo\"") should be (JString("woo"))
    }

    "is parsable from a JSON string as a JValue" - {
      parse[JValue]("\"woo\"") should be (JString("woo"))
    }
  }

  "An AST.JNull" - {
    "generates a JSON null" - {
      generate(JNull) should be ("null")
    }

    "is parsable from a JSON null" - {
      parse[JNull.type]("null") should be (JNull)
    }

    "is parsable from a JSON null as a JValue" - {
      parse[JValue]("null") should be (JNull)
    }
  }

  "An AST.JBoolean" - {
    "generates a JSON true" - {
      generate(JBoolean(true)) should be ("true")
    }

    "generates a JSON false" - {
      generate(JBoolean(false)) should be ("false")
    }

    "is parsable from a JSON true" - {
      parse[JBoolean]("true") should be (JBoolean(true))
    }

    "is parsable from a JSON false" - {
      parse[JBoolean]("false") should be (JBoolean(false))
    }

    "is parsable from a JSON true as a JValue" - {
      parse[JValue]("true") should be (JBoolean(true))
    }

    "is parsable from a JSON false as a JValue" - {
      parse[JValue]("false") should be (JBoolean(false))
    }
  }

  "An AST.JArray of JInts" - {
    "generates a JSON array of ints" - {
      generate(JArray(List(JInt(1), JInt(2), JInt(3)))) should be ("[1,2,3]")
    }

    "is parsable from a JSON array of ints" - {
      parse[JArray]("[1,2,3]") should be (JArray(List(JInt(1), JInt(2), JInt(3))))
    }

    "is parsable from a JSON array of ints as a JValue" - {
      parse[JValue]("[1,2,3]") should be (JArray(List(JInt(1), JInt(2), JInt(3))))
    }
  }

  "An AST.JObject" - {
    val obj = JObject(List(JField("id", JInt(1)), JField("name", JString("Coda"))))

    "generates a JSON object with matching field values" - {
      generate(obj) should be ("""{"id":1,"name":"Coda"}""")
    }

    "is parsable from a JSON object" - {
      parse[JObject]("""{"id":1,"name":"Coda"}""") should be (obj)
    }

    "is parsable from a JSON object as a JValue" - {
      parse[JValue]("""{"id":1,"name":"Coda"}""") should be (obj)
    }

    "is parsable from an empty JSON object" - {
      parse[JObject]("""{}""") should be (JObject(Nil))
    }
  }
}
