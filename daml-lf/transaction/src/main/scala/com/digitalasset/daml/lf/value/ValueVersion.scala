// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.daml.lf.value

import com.digitalasset.daml.lf.value.Value._
import com.digitalasset.daml.lf.LfVersions
import com.digitalasset.daml.lf.data.{FrontStack, FrontStackCons, ImmArray}

import scala.annotation.tailrec

final case class ValueVersion(protoValue: String)

/**
  * Currently supported versions of the DAML-LF value specification.
  */
object ValueVersions
    extends LfVersions(
      maxVersion = ValueVersion("5"),
      previousVersions = List("1", "2", "3", "4") map ValueVersion)(_.protoValue) {

  private[this] val minVersion = ValueVersion("1")
  private[this] val minOptional = ValueVersion("2")
  private[value] val minContractIdStruct = ValueVersion("3")
  private[this] val minMap = ValueVersion("4")
  private[this] val minEnum = ValueVersion("5")

  def assignVersion[Cid, Status](v0: Value[Cid, Status]): Either[String, ValueVersion] = {
    import com.digitalasset.daml.lf.transaction.VersionTimeline.{maxVersion => maxVV}

    @tailrec
    def go(
        currentVersion: ValueVersion,
        values0: FrontStack[Value[Cid, Status]]): Either[String, ValueVersion] = {
      if (currentVersion == maxVersion) {
        Right(currentVersion)
      } else {
        values0 match {
          case FrontStack() => Right(currentVersion)
          case FrontStackCons(value, values) =>
            value match {
              // for things supported since version 1, we do not need to check
              case ValueRecord(_, fs) => go(currentVersion, fs.map(v => v._2) ++: values)
              case ValueVariant(_, _, arg) => go(currentVersion, arg +: values)
              case ValueList(vs) => go(currentVersion, vs.toImmArray ++: values)
              case ValueContractId(_) | ValueInt64(_) | ValueDecimal(_) | ValueText(_) |
                  ValueTimestamp(_) | ValueParty(_) | ValueBool(_) | ValueDate(_) | ValueUnit() =>
                go(currentVersion, values)
              // for things added after version 1, we raise the minimum if present
              case ValueOptional(x) =>
                go(maxVV(minOptional, currentVersion), ImmArray(x.toList) ++: values)
              case ValueMap(map) =>
                go(maxVV(minMap, currentVersion), map.values ++: values)
              case ValueEnum(_, _) =>
                go(maxVV(minEnum, currentVersion), values)
              // tuples are a no-no
              case ValueTuple(fields) =>
                Left(s"Got tuple when trying to assign version. Fields: $fields")
            }
        }
      }
    }

    go(minVersion, FrontStack(v0))
  }

  @throws[IllegalArgumentException]
  def assertAssignVersion[Cid](v0: Value[Cid, NotTyped]): ValueVersion =
    assignVersion(v0) match {
      case Left(err) => throw new IllegalArgumentException(err)
      case Right(x) => x
    }
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def asVersionedValue[Cid, Status](
      value: Value[Cid, Status]): Either[String, VersionedValue[Cid, Status]] =
    assignVersion(value).map(version =>
      VersionedValue[Cid, Status](version = version, value = value))

  @throws[IllegalArgumentException]
  def assertAsVersionedValue[Cid, Status](value: Value[Cid, Status]): VersionedValue[Cid, Status] =
    asVersionedValue(value) match {
      case Left(err) => throw new IllegalArgumentException(err)
      case Right(x) => x
    }
}
