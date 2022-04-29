module Data.ArrayTest

import System.FFI
import Data.Vect

import Test.Unit

import Data.Array.PrimitivesTest

public export
tests : PrimIO es => List (Test es)
tests = Data.Array.PrimitivesTest.tests
