package com.codahale.jerkson.tests
import org.scalatest.{Matchers, FreeSpec}
import com.codahale.jerkson.AST.{JInt, JString, JValue, JNull}
import com.codahale.jerkson.Json._

class JValueSpec extends FreeSpec with Matchers {

  "Selecting single nodes" - {
    "returns None with primitives" in {
      (parse[JValue]("8") \ "blah") should be (JNull)
    }
    "returns None on nonexistent fields" in {
      (parse[JValue]("{\"one\": \"1\"}") \ "two") should be (JNull)
    }

    "returns a JValue with an existing field" in {
      (parse[JValue]("{\"one\": \"1\"}") \ "one") should be (JString("1"))
    }
  }

  "Selecting array members" - {
    "returns None with primitives" in {
      parse[JValue]("\"derp\"").apply(0) should be (JNull)
    }

    "returns None on out of bounds" - {
      parse[JValue]("[0, 1, 2, 3]").apply(4) should be(JNull)
    }

    "returns a JValue" - {
      parse[JValue]("[0, 1, 2, 3]").apply(2) should be(JInt(2))
    }
  }

  "Deep selecting"  - {
    "returns Nil with primitives" - {
      (parse[JValue]("0.234") \\ "herp") should be (empty)
    }

    "returns Nil on nothing found" - {
      (parse[JValue]("{\"one\": {\"two\" : \"three\"}}") \\ "four") should be (empty)
    }

    "returns single leaf nodes" - {
      (parse[JValue]("{\"one\": {\"two\" : \"three\"}}") \\ "two") should be (Seq(JString("three")))
    }

    "should return multiple leaf nodes" - {
      (parse[JValue]("{\"one\": {\"two\" : \"three\"}, \"four\": {\"two\" : \"five\"}}") \\ "two") should be (Seq(JString("three"),JString("five")))
    }
  }
}
