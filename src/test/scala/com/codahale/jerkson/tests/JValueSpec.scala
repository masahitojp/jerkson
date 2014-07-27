package com.codahale.jerkson.tests
import org.scalatest.FreeSpec
import com.codahale.jerkson.AST.{JString, JValue, JNull}
import com.codahale.jerkson.Json._

class JValueSpec extends FreeSpec {

    "Selecting single nodes" - {
      "returns None with primitives" in {
        assert((parse[JValue]("8") \ "blah") === JNull)
      }
      "returns None on nonexistent fields" in {
        assert((parse[JValue]("{\"one\": \"1\"}") \ "two") === JNull)
      }

      "returns a JValue with an existing field" in {
        assert((parse[JValue]("{\"one\": \"1\"}") \ "one") === JString("1"))
      }
      "should produce NoSuchElementException when head is invoked" in {
        intercept[NoSuchElementException] {
          Set.empty.head
        }
      }
    }

}
