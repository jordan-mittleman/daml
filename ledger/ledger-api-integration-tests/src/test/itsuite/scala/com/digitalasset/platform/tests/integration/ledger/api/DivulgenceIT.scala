// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.platform.tests.integration.ledger.api

import java.util.UUID

import akka.stream.scaladsl.Sink
import com.digitalasset.daml.lf.data.Ref.{ContractIdString, LedgerString}
import com.digitalasset.daml.lf.data.{ImmArray, Ref}
import com.digitalasset.daml.lf.value.Value
import com.digitalasset.daml.lf.value.Value.{
  AbsoluteContractId,
  ValueContractId,
  ValueParty,
  ValueRecord
}
import com.digitalasset.ledger.api.testing.utils.{
  AkkaBeforeAndAfterAll,
  SuiteResourceManagementAroundAll
}
import com.digitalasset.ledger.api.v1
import com.digitalasset.ledger.api.v1.command_submission_service.SubmitRequest
import com.digitalasset.ledger.api.v1.commands._
import com.digitalasset.ledger.api.v1.transaction_filter._
import com.digitalasset.ledger.client.services.acs.ActiveContractSetClient
import com.digitalasset.ledger.client.services.transactions.TransactionClient
import com.digitalasset.platform.apitesting.{
  LedgerContext,
  MultiLedgerFixture,
  TestIdsGenerator,
  TestTemplateIds
}
import com.digitalasset.platform.participant.util.LfEngineToApi
import com.google.protobuf.timestamp.Timestamp
import org.scalatest.Inside.inside
import org.scalatest.concurrent.{AsyncTimeLimitedTests, ScalaFutures}
import org.scalatest.{AsyncFlatSpec, Matchers, OptionValues}
import scalaz.syntax.tag._

import scala.concurrent.Future
import scala.language.implicitConversions

class DivulgenceIT
    extends AsyncFlatSpec
    with AkkaBeforeAndAfterAll
    with MultiLedgerFixture
    with SuiteResourceManagementAroundAll
    with ScalaFutures
    with AsyncTimeLimitedTests
    with Matchers
    with OptionValues {
  override protected def config: Config = Config.default

  protected val testTemplateIds = new TestTemplateIds(config)
  protected val templateIds = testTemplateIds.templateIds
  protected val testIdsGenerator = new TestIdsGenerator(config)

  private lazy val alice = testIdsGenerator.testPartyName("alice")
  private lazy val bob = testIdsGenerator.testPartyName("bob")

  private implicit def party(s: String): Ref.Party = Ref.Party.assertFromString(s)
  private implicit def pkgId(s: String): Ref.PackageId = Ref.PackageId.assertFromString(s)
  private implicit def id(s: String): Ref.Name = Ref.Name.assertFromString(s)

  private def acsClient(ctx: LedgerContext): ActiveContractSetClient =
    new ActiveContractSetClient(ctx.ledgerId, ctx.acsService)

  private def transactionClient(ctx: LedgerContext): TransactionClient =
    new TransactionClient(ctx.ledgerId, ctx.transactionService)

  private val ledgerEffectiveTime = Timestamp(0L, 0)
  private val maximumRecordTime =
    ledgerEffectiveTime.copy(seconds = ledgerEffectiveTime.seconds + 30L)

  private def submitRequest(
      ctx: LedgerContext,
      commandId: String,
      workflowId: String,
      party: String): SubmitRequest =
    SubmitRequest(
      commands = Some(
        Commands(
          ledgerId = ctx.ledgerId.unwrap,
          workflowId = workflowId,
          applicationId = "divulgence-test-application-id",
          commandId = commandId,
          party = party,
          Some(ledgerEffectiveTime),
          Some(maximumRecordTime),
          commands = Nil,
        ))
    )

  private def create(
      ctx: LedgerContext,
      commandId: String,
      workflowId: String,
      party: String,
      tpl: v1.value.Identifier,
      arg: ValueRecord[AbsoluteContractId]): Future[ContractIdString] =
    for {
      ledgerEndBeforeSubmission <- transactionClient(ctx).getLedgerEnd.map(_.getOffset)
      client <- ctx.commandClient()
      _ <- client.submitSingleCommand(
        submitRequest(ctx, commandId, workflowId, party).update(
          _.commands.commands := List(
            Command(
              Command.Command.Create(CreateCommand(
                templateId = Some(tpl),
                createArguments = Some(LfEngineToApi.lfValueToApiRecord(true, arg).right.get)
              ))))
        ))
      transaction <- transactionClient(ctx)
        .getTransactions(
          ledgerEndBeforeSubmission,
          None,
          TransactionFilter(Map(party -> Filters.defaultInstance)))
        .filter(_.commandId == commandId)
        .runWith(Sink.head)
    } yield
      LedgerString.assertFromString(
        transaction.events.map(_.event).head.created.toList.head.contractId
      )

  private def exercise(
      ctx: LedgerContext,
      commandId: String,
      workflowId: String,
      party: String,
      tpl: v1.value.Identifier,
      contractId: String,
      choice: String,
      arg: Value[AbsoluteContractId]): Future[Unit] =
    for {
      ledgerEndBeforeSubmission <- transactionClient(ctx).getLedgerEnd.map(_.getOffset)
      client <- ctx.commandClient()
      _ <- client.submitSingleCommand(
        submitRequest(ctx, commandId, workflowId, party).update(
          _.commands.commands := List(
            Command(
              Command.Command.Exercise(
                ExerciseCommand(
                  templateId = Some(tpl),
                  contractId = contractId,
                  choice = choice,
                  choiceArgument = Some(LfEngineToApi.lfValueToApiValue(true, arg).right.get)
                ))
            )
          )
        )
      )

      _ <- transactionClient(ctx)
        .getTransactions(
          ledgerEndBeforeSubmission,
          None,
          TransactionFilter(Map(party -> Filters.defaultInstance)))
        .filter(_.commandId == commandId)
        .runWith(Sink.head)
    } yield ()

  // here div1Cid is _divulged_ to Bob after it is created. the checks below
  // check that this divulgence is not visible in the ACS / flat transaction stream,
  // but that it is visible in the transaction trees.
  case class Setup(div1Cid: String, div2Cid: String)

  private def createDivulgence1(ctx: LedgerContext, workflowId: String): Future[ContractIdString] =
    create(
      ctx,
      testIdsGenerator.testCommandId(s"create-Divulgence1-${UUID.randomUUID()}"),
      workflowId,
      alice,
      templateIds.divulgence1,
      ValueRecord(None, ImmArray(Some[Ref.Name]("div1Party") -> ValueParty(alice)))
    )

  private def createDivulgence2(ctx: LedgerContext, workflowId: String): Future[ContractIdString] =
    create(
      ctx,
      testIdsGenerator.testCommandId(s"create-Divulgence2-${UUID.randomUUID()}"),
      workflowId,
      bob,
      templateIds.divulgence2,
      ValueRecord(
        None,
        ImmArray(
          Some[Ref.Name]("div2Signatory") -> ValueParty(bob),
          Some[Ref.Name]("div2Fetcher") -> ValueParty(alice)))
    )

  private def divulgeViaFetch(
      ctx: LedgerContext,
      div1Cid: ContractIdString,
      div2Cid: ContractIdString,
      workflowId: String): Future[Unit] =
    exercise(
      ctx,
      testIdsGenerator.testCommandId(s"exercise-Divulgence2Fetch-${UUID.randomUUID()}"),
      workflowId,
      alice,
      templateIds.divulgence2,
      div2Cid,
      "Divulgence2Fetch",
      ValueRecord(
        None,
        ImmArray(Some[Ref.Name]("div1ToFetch") -> ValueContractId(AbsoluteContractId(div1Cid))))
    )

  private def divulgeViaArchive(
      ctx: LedgerContext,
      div1Cid: ContractIdString,
      div2Cid: ContractIdString,
      workflowId: String): Future[Unit] =
    exercise(
      ctx,
      testIdsGenerator.testCommandId(s"exercise-Divulgence2Archive-${UUID.randomUUID()}"),
      workflowId,
      alice,
      templateIds.divulgence2,
      div2Cid,
      "Divulgence2Archive",
      ValueRecord(
        None,
        ImmArray(Some[Ref.Name]("div1ToArchive") -> ValueContractId(AbsoluteContractId(div1Cid))))
    )

  private val bobFilter = TransactionFilter(Map(bob -> Filters.defaultInstance))
  private val bothFilter = TransactionFilter(
    Map(alice -> Filters.defaultInstance, bob -> Filters.defaultInstance))

  behavior of "Divulgence"

  it should "not expose divulged contracts in flat stream" in allFixtures { ctx =>
    val wfid = testIdsGenerator.testWorkflowId("divulgence-test-flat-stream-workflow-id")
    for {
      beforeTest <- transactionClient(ctx).getLedgerEnd.map(_.getOffset)
      div1Cid <- createDivulgence1(ctx, wfid)
      div2Cid <- createDivulgence2(ctx, wfid)
      _ <- divulgeViaArchive(ctx, div1Cid, div2Cid, wfid)
      ledgerEnd <- transactionClient(ctx).getLedgerEnd.map(_.getOffset)
      bobFlatTransactions <- transactionClient(ctx)
        .getTransactions(
          beforeTest,
          Some(ledgerEnd),
          bobFilter
        )
        .runWith(Sink.seq)
        .map(_.toList)
      bothFlatTransactions <- transactionClient(ctx)
        .getTransactions(
          beforeTest,
          Some(ledgerEnd),
          bothFilter
        )
        .runWith(Sink.seq)
        .map(_.toList)
      bobTreeTransactions <- transactionClient(ctx)
        .getTransactionTrees(
          beforeTest,
          Some(ledgerEnd),
          bobFilter
        )
        .runWith(Sink.seq)
        .map(_.toList)
    } yield {
      // first what we expect for Bob's flat transactions
      {
        // we expect only one transaction, containing only one create event for Divulgence2.
        // we do _not_ expect the create or archive for Divulgence1, even if Divulgence1 was divulged
        // to bob, and even if the exercise is visible to bob in the transaction trees.
        bobFlatTransactions should have length 1
        val events = bobFlatTransactions.head.events
        events should have length 1
        val event = events.head.event
        inside(event.created) {
          case Some(created) =>
            created.contractId shouldBe div2Cid
        }
      }
      // then what we expect for Bob's tree transactions. note that here we witness the exercise that
      // caused the archive of div1Cid, even if we did _not_ see the archive event in the flat transaction
      // stream above
      {
        // we should get two transactions -- one for the second create and one for the exercise.
        bobTreeTransactions should have length 2
        val div2CreateTx = bobTreeTransactions(0)
        div2CreateTx.rootEventIds should have length 1
        val div2CreateEvent = div2CreateTx.eventsById(div2CreateTx.rootEventIds.head)
        inside(div2CreateEvent.kind.created) {
          case Some(created) =>
            created.contractId shouldBe div2Cid
        }
        val div2ExerciseTx = bobTreeTransactions(1)
        div2ExerciseTx.rootEventIds should have length 1
        val div2ExerciseEvent = div2ExerciseTx.eventsById(div2ExerciseTx.rootEventIds.head)
        inside(div2ExerciseEvent.kind.exercised) {
          case Some(div2Exercise) =>
            div2Exercise.contractId shouldBe div2Cid
            div2Exercise.childEventIds should have length 1
            val div1ExerciseEvent = div2ExerciseTx.eventsById(div2Exercise.childEventIds.head)
            inside(div1ExerciseEvent.kind.exercised) {
              case Some(div1Exercise) =>
                div1Exercise.contractId shouldBe div1Cid
                div1Exercise.childEventIds should have length 0
            }
        }
      }
      {
        // alice sees:
        // * create Divulgence1
        // * create Divulgence2
        // * archive Divulgence1
        // note that we do _not_ see the exercise of Divulgence2 because it is nonconsuming
        bothFlatTransactions should have length 3
        bothFlatTransactions.head.events should have length 1
        val div1CreateEvent = bothFlatTransactions.head.events.head
        inside(div1CreateEvent.event.created) {
          case Some(created) =>
            created.contractId shouldBe div1Cid
            created.witnessParties.toList shouldBe List(alice) // bob does not see
        }
      }
    }
  }

  it should "not expose divulged contracts in ACS" in allFixtures { ctx =>
    val wfid = testIdsGenerator.testWorkflowId("divulgence-test-acs-workflow-id")
    for {
      div1Cid <- createDivulgence1(ctx, wfid)
      div2Cid <- createDivulgence2(ctx, wfid)
      _ <- divulgeViaFetch(ctx, div1Cid, div2Cid, wfid)
      bobEvents <- acsClient(ctx)
        .getActiveContracts(bobFilter)
        .filter(_.workflowId == wfid)
        .runWith(Sink.seq)
        .map { acsResps =>
          List.concat(acsResps.map(_.activeContracts): _*)
        }
      bothEvents <- acsClient(ctx)
        .getActiveContracts(bothFilter)
        .filter(_.workflowId == wfid)
        .runWith(Sink.seq)
        .map { acsResps =>
          List.concat(acsResps.map(_.activeContracts): _*)
        }
    } yield {
      // bob only sees divulgence 2
      {
        bobEvents should have length 1
        bobEvents.head.contractId shouldBe div2Cid
        // note that since the bob filter only concerns bob, here we get only bob as witness, even if alice sees
        // this contract too.
        bobEvents.head.witnessParties.toList.sorted shouldBe List(bob)
      }
      // alice sees both
      {
        bothEvents should have length 2
        bothEvents.map(_.contractId: String).sorted shouldBe List[String](div1Cid, div2Cid).sorted
        bothEvents.find(_.contractId == div1Cid).map(_.witnessParties.toList) shouldBe Some(
          List(alice))
        bothEvents.find(_.contractId == div2Cid).map(_.witnessParties.toList.sorted) shouldBe Some(
          List(alice, bob))
      }
    }
  }
}
