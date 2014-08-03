package com.codahale.jerkson.tests

import com.codahale.jerkson.Json._
import com.codahale.jerkson.ParsingException
import java.io.ByteArrayInputStream
import org.scalatest.{FreeSpec, ShouldMatchers}

class EdgeCaseSpec extends FreeSpec with ShouldMatchers {
  "Deserializing lists" - {
    "doesn't cache Seq builders" - {
      parse[List[Int]]("[1,2,3,4]") should be (List(1, 2, 3, 4))
      parse[List[Int]]("[1,2,3,4]") should be (List(1, 2, 3, 4))
    }
  }

  "Parsing a JSON array of ints with nulls" - {
    "should be readable as a List[Option[Int]]" - {
      parse[List[Option[Int]]]("[1,2,null,4]") should be (List(Some(1), Some(2), None, Some(4)))
    }
  }

  "Deserializing maps" - {
    "doesn't cache Map builders" - {
      parse[Map[String, Int]](""" {"one":1, "two": 2} """) should be (Map("one" -> 1, "two" -> 2))
      parse[Map[String, Int]](""" {"one":1, "two": 2} """) should be (Map("one" -> 1, "two" -> 2))
    }
  }

  "Parsing malformed JSON" - {
    "should throw a ParsingException with an informative message" - {
      intercept[ParsingException] {
        parse[Boolean]("jjf8;09")
      }.getMessage should be (
            "Malformed JSON. Unrecognized token 'jjf8': was expecting "+
              "('true', 'false' or 'null') at character offset 4.")

      intercept[ParsingException] {
        parse[CaseClass]("{\"ye\":1")
      }.getMessage should be (
            "Malformed JSON. Unexpected end-of-input: expected close marker for " +
                    "OBJECT at character offset 21.")
    }
  }

  "Parsing invalid JSON" - {
    "should throw a ParsingException with an informative message" - {
      intercept[ParsingException] {
        parse[CaseClass]("900")
      }.getMessage.split("\n")(0) should be (
        "Can not deserialize instance of com.codahale.jerkson.tests.CaseClass out of VALUE_NUMBER_INT token"
      )

      intercept[ParsingException] {
        parse[CaseClass]("{\"woo\": 1}")
      }.getMessage should be ("Invalid JSON. Needed [id, name], but found [woo].")
    }
  }

  "Parsing an empty document" - {
    "should throw a ParsingException with an informative message" - {
      val input = new ByteArrayInputStream(Array.empty)
      intercept[ParsingException] {
        parse[CaseClass](input)
      }.getMessage.split("\n")(0) should be ("No content to map due to end-of-input")
    }
  }
}
