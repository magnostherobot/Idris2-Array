module ArrayTest

import System.FFI
import Data.Vect

import Test.Unit

import Array

dummyPtr : HasIO io => io AnyPtr
dummyPtr = malloc 0

public export
tests : PrimIO es => List (Test es)
tests = [ MkTest "newArray doesn't crash" $ do
          _ <- newArray ()
          pass
        
        , MkTest "append doesn't crash" $ do
          ptr <- dummyPtr
          arr <- newArray AnyPtr
          arr' <- append arr ptr
          pass

        , MkTest "index doesn't crash" $ do
          arr <- newArray AnyPtr
          ptr <- dummyPtr
          arr <- append arr ptr
          let _ = index 0 arr
          pass

        , MkTest "toArray doesn't crash" $ do
          let vect : Vect _ Int = [1, 2, 3, 4, 5]
          _ <- toArray vect
          pass

        , MkTest "toArray places elements in correct order" $ do
          let vect : Vect _ Int = [1, 2, 3, 4, 5]
          arr <- toArray vect
          assertEq (index 0 vect) (index 0 arr)
          assertEq (index 1 vect) (index 1 arr)
          assertEq (index 2 vect) (index 2 arr)
          assertEq (index 3 vect) (index 3 arr)
          assertEq (index 4 vect) (index 4 arr)
        ]
