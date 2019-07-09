-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE GADTs #-}

module DA.Ledger.Services.ActiveContractsService (getActiveContracts) where

import Com.Digitalasset.Ledger.Api.V1.ActiveContractsService
import Com.Digitalasset.Ledger.Api.V1.TransactionFilter --TODO: HL mirror
import Control.Concurrent (forkIO)
import DA.Ledger.Convert
import DA.Ledger.GrpcWrapUtils
import DA.Ledger.LedgerService
import DA.Ledger.Stream
import DA.Ledger.Types
import Network.GRPC.HighLevel.Generated

type Response = (AbsOffset,Maybe WorkflowId,[Event]) -- Always CreatedEvent. TODO: Improve types to enforce.

getActiveContracts :: LedgerId -> TransactionFilter -> Verbosity -> LedgerService [Response]
getActiveContracts lid tf verbosity =
    makeLedgerService $ \timeout config -> do
    stream <- newStream
    let request = mkRequest lid tf verbosity
    _ <- forkIO $
        withGRPCClient config $ \client -> do
            service <- activeContractsServiceClient client
            let ActiveContractsService {activeContractsServiceGetActiveContracts=rpc} = service
            sendToStream timeout request raiseGetActiveContractsResponse stream rpc
    -- The stream is not continuous, so we force the force the list here.
    streamToList stream

mkRequest :: LedgerId -> TransactionFilter -> Verbosity -> GetActiveContractsRequest
mkRequest lid tf verbosity = GetActiveContractsRequest
    { getActiveContractsRequestLedgerId = unLedgerId lid
    , getActiveContractsRequestFilter = Just tf
    , getActiveContractsRequestVerbose = unVerbosity verbosity
    , getActiveContractsRequestTraceContext = noTrace
    }
