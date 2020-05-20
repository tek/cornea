{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework
import Test.Framework.BlackBoxTest ()
import {-@ HTF_TESTS @-} DeepErrorSpec
import {-@ HTF_TESTS @-} DeepReaderSpec
import {-@ HTF_TESTS @-} DeepStateSpec

main :: IO ()
main = htfMain htf_importedTests
