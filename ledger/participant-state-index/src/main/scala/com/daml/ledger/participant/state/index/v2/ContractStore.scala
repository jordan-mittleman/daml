// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.participant.state.index.v2

import com.digitalasset.daml.lf.data.Ref
import com.digitalasset.daml.lf.data.Ref.Party
import com.digitalasset.daml.lf.transaction.Node.GlobalKey
import com.digitalasset.daml.lf.value.Value

import scala.concurrent.Future

/**
  * Meant be used for optimistic contract lookups before command submission.
  */
trait ContractStore {
  def lookupActiveContract(
      submitter: Ref.Party,
      contractId: Value.AbsoluteContractId
  ): Future[
    Option[Value.ContractInst[Value.VersionedValue[Value.AbsoluteContractId, Value.WellTyped]]]]

  def lookupContractKey(submitter: Party, key: GlobalKey): Future[Option[Value.AbsoluteContractId]]

}
