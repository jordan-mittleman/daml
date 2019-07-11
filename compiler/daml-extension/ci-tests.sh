#!/usr/bin/env bash
# Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

set -euo pipefail

cd compiler/daml-extension

: Checking JSON files for proper syntax
for sf in *.json syntaxes/*.json; do
  cat $sf | jq '.'
done
