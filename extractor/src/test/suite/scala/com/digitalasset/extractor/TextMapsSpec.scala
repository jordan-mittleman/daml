// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.extractor

import java.io.File

import com.digitalasset.daml.bazeltools.BazelRunfiles._
import com.digitalasset.extractor.services.{CustomMatchers, ExtractorFixtureAroundAll}
import com.digitalasset.ledger.api.testing.utils.SuiteResourceManagementAroundAll
import com.digitalasset.testing.postgresql.PostgresAroundAll
import io.circe.parser._
import org.scalatest._
import scalaz.Scalaz._

@SuppressWarnings(Array("org.wartremover.warts.Any"))
class TextMapsSpec
    extends FlatSpec
    with Suite
    with PostgresAroundAll
    with SuiteResourceManagementAroundAll
    with ExtractorFixtureAroundAll
    with Inside
    with Matchers
    with CustomMatchers {

  override protected def darFile = new File(rlocation("extractor/PrimitiveTypes.dar"))

  override def scenario: Option[String] = Some("PrimitiveTypes:textMaps")

  "TextMaps" should "be extracted" in {
    val contracts = getContracts

    contracts should have length 2
  }

  it should "contain the correct JSON data" in {
    val contractsJson = getContracts.map(_.create_arguments)

    val expected = List(
      """
        {
          "reference" : "Empty maps",
          "map" : {},
          "deep_map" : {},
          "party" : "Bob"
        }
      """,
      """
        {
         "reference" : "Non-empty maps",
         "map" : { "1" : 1 ,
                   "2" : 2 ,
                   "3" : 3 ,
                   "4" : 4 ,
                   "5" : 5 },
         "deep_map" : {},
         "party" : "Bob"
        }
      """
    ).traverseU(parse)

    expected should be('right) // That should only fail if this JSON^^ is ill-formatted

    contractsJson should contain theSameElementsAs expected.right.get
  }
}