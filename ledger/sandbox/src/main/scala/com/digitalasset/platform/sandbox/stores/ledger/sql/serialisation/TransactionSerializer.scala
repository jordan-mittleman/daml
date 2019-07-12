// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
package com.digitalasset.platform.sandbox.stores.ledger.sql.serialisation

import com.digitalasset.daml.lf.archive.{Decode, Reader}
import com.digitalasset.daml.lf.data.Ref
import com.digitalasset.daml.lf.transaction._
import com.digitalasset.daml.lf.value.Value.{AbsoluteContractId, WellTyped, VersionedValue}
import com.digitalasset.daml.lf.value.ValueCoder.{DecodeError, EncodeError}
import com.digitalasset.ledger.EventId

trait TransactionSerializer {

  def serialiseTransaction(
      transaction: GenTransaction[
        EventId,
        AbsoluteContractId,
        VersionedValue[AbsoluteContractId, WellTyped]]): Either[EncodeError, Array[Byte]]

  def deserializeTransaction(blob: Array[Byte]): Either[
    DecodeError,
    GenTransaction[EventId, AbsoluteContractId, VersionedValue[AbsoluteContractId, WellTyped]]]

}

object TransactionSerializer extends TransactionSerializer {

  private val defaultNidEncode: TransactionCoder.EncodeNid[EventId] = identity
  private def defaultDecodeNid(s: String) =
    Ref.LedgerString
      .fromString(s)
      .left
      .map(
        e => DecodeError(s"cannot decode noid: $e")
      )

  override def serialiseTransaction(
      transaction: GenTransaction[
        EventId,
        AbsoluteContractId,
        VersionedValue[AbsoluteContractId, WellTyped]]): Either[EncodeError, Array[Byte]] =
    TransactionCoder
      .encodeTransactionWithCustomVersion(
        defaultNidEncode,
        ContractSerializer.defaultCidEncode,
        VersionedTransaction(
          TransactionVersions.assignVersion(transaction),
          transaction
        )
      )
      .map(_.toByteArray())

  override def deserializeTransaction(blob: Array[Byte]): Either[
    DecodeError,
    GenTransaction[EventId, AbsoluteContractId, VersionedValue[AbsoluteContractId, WellTyped]]] =
    TransactionCoder
      .decodeVersionedTransaction(
        defaultDecodeNid,
        ContractSerializer.defaultCidDecode,
        TransactionOuterClass.Transaction.parseFrom(
          Decode.damlLfCodedInputStreamFromBytes(blob, Reader.PROTOBUF_RECURSION_LIMIT))
      )
      .map(_.transaction)

}
