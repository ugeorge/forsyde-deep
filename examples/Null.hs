{-# LANGUAGE TemplateHaskell #-}

-- This module is aimed at testing the, normally useless but possible, 
-- null system

module Null where

import ForSyDe

nullSysF :: ()
nullSysF = ()

nullSysDef :: SysDef ()
nullSysDef = $(newSysDef 'nullSysF [] [])

nullIns0 :: ()
nullIns0 = $(instantiate "null0" 'nullSysDef)

simNull :: ()
simNull = $(simulate 'nullSysDef) 