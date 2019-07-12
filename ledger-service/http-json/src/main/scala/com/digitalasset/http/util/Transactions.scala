// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.http.util

import com.digitalasset.ledger.api.v1.event.{ArchivedEvent, CreatedEvent, Event}
import com.digitalasset.ledger.api.v1.transaction.Transaction

object Transactions {
  def decodeCreatedEvent(transaction: Transaction): Option[CreatedEvent] =
    for {
      event <- transaction.events.headOption: Option[Event]
      created <- event.event.created: Option[CreatedEvent]
    } yield created

  def decodeArchivedEvent(transaction: Transaction): Option[ArchivedEvent] = {
    for {
      event <- transaction.events.headOption: Option[Event]
      archived <- event.event.archived: Option[ArchivedEvent]
    } yield archived
  }
}