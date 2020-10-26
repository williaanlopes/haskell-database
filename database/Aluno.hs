{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Aluno where

import Data.Text as Text
    
data Aluno = Aluno { _id :: Integer, _nome :: Text, _cpf :: Text, _nota :: Float } deriving ( Eq, Ord, Read, Show )
