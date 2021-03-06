package com.codahale.jerkson.tests

import java.net.URI
import com.codahale.jerkson.Json._
import java.util.UUID
import org.scalatest.{Matchers, FreeSpec}

class FancyTypeSupportSpec extends FreeSpec with Matchers {
  "A URI" - {
    "generates a JSON string" - {
      generate(new URI("http://example.com/resource?query=yes")) should be ("\"http://example.com/resource?query=yes\"")
    }

    "is parsable from a JSON string" - {
      parse[URI]("\"http://example.com/resource?query=yes\"") should be (new URI("http://example.com/resource?query=yes"))
    }
  }

  "A UUID" - {
    val uuid = UUID.fromString("a62047e4-bfb5-4d71-aad7-1a6b338eee63")

    "generates a JSON string" - {
      generate(uuid) should be ("\"a62047e4-bfb5-4d71-aad7-1a6b338eee63\"")
    }

    "is parsable from a JSON string" - {
      parse[UUID]("\"a62047e4-bfb5-4d71-aad7-1a6b338eee63\"") should be (uuid)
    }
  }
}
