// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.daml.lf.engine.trigger

import akka.actor.ActorSystem
import akka.stream._
import java.io.File
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scalaz.syntax.traverse._

import com.digitalasset.daml.lf.archive.{Dar, DarReader}
import com.digitalasset.daml.lf.archive.Decode
import com.digitalasset.daml.lf.data.Ref.{Identifier, PackageId, QualifiedName}
import com.digitalasset.daml.lf.language.Ast._
import com.digitalasset.daml_lf_dev.DamlLf
import com.digitalasset.grpc.adapter.AkkaExecutionSequencerPool
import com.digitalasset.ledger.api.refinements.ApiTypes.ApplicationId
import com.digitalasset.ledger.client.LedgerClient
import com.digitalasset.ledger.client.configuration.{
  CommandClientConfiguration,
  LedgerClientConfiguration,
  LedgerIdRequirement
}
import com.digitalasset.auth.TokenHolder

object RunnerMain {

  def listTriggers(darPath: File, dar: Dar[(PackageId, Package)]) = {
    val triggerIds = TriggerIds.fromDar(dar)
    println(s"Listing triggers in $darPath:")
    for ((modName, mod) <- dar.main._2.modules) {
      for ((defName, defVal) <- mod.definitions) {
        defVal match {
          case DValue(TApp(TTyCon(tcon), _), _, _, _) => {
            if (tcon == triggerIds.getHighlevelId("Trigger")
              || tcon == triggerIds.getId("Trigger")) {
              println(s"  $modName:$defName")
            }
          }
          case _ => {}
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {

    RunnerConfig.parse(args) match {
      case None => sys.exit(1)
      case Some(config) => {
        val encodedDar: Dar[(PackageId, DamlLf.ArchivePayload)] =
          DarReader().readArchiveFromFile(config.darPath.toFile).get
        val dar: Dar[(PackageId, Package)] = encodedDar.map {
          case (pkgId, pkgArchive) => Decode.readArchivePayload(pkgId, pkgArchive)
        }

        if (config.listTriggers) {
          listTriggers(config.darPath.toFile, dar)
          sys.exit(0)
        }

        val triggerId: Identifier =
          Identifier(dar.main._1, QualifiedName.assertFromString(config.triggerIdentifier))

        val system: ActorSystem = ActorSystem("TriggerRunner")
        implicit val materializer: Materializer = Materializer(system)
        val sequencer = new AkkaExecutionSequencerPool("TriggerRunnerPool")(system)
        implicit val ec: ExecutionContext = system.dispatcher

        val tokenHolder = config.accessTokenFile.map(new TokenHolder(_))
        // We probably want to refresh the token at some point but given that triggers
        // are expected to be written such that they can be killed and restarted at
        // any time it would in principle also be fine to just have the auth failure due
        // to an expired token tear the trigger down and have some external monitoring process (e.g. systemd)
        // restart it.
        val applicationId = ApplicationId("Trigger Runner")
        val clientConfig = LedgerClientConfiguration(
          applicationId = ApplicationId.unwrap(applicationId),
          ledgerIdRequirement = LedgerIdRequirement("", enabled = false),
          commandClient = CommandClientConfiguration.default.copy(ttl = config.commandTtl),
          sslContext = None,
          token = tokenHolder.flatMap(_.token)
        )

        val flow: Future[Unit] = for {
          client <- LedgerClient.singleHost(config.ledgerHost, config.ledgerPort, clientConfig)(
            ec,
            sequencer)
          _ <- Runner.run(
            dar,
            triggerId,
            client,
            config.timeProviderType,
            applicationId,
            config.ledgerParty)
        } yield ()

        flow.onComplete(_ => system.terminate())

        Await.result(flow, Duration.Inf)
      }
    }
  }
}