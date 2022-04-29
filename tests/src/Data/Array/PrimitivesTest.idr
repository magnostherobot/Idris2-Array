module Data.Array.PrimitivesTest

import System.FFI
import Test.Unit

import Data.Array.Primitives

%hide Control.App.primIO

dummyPtr : HasIO io => io AnyPtr
dummyPtr = malloc 0

public export
tests : PrimIO es => List (Test es)
tests = [ MkTest "prim__arrayAppendAnyPtr doesn't crash" $ do
          a <- primIO prim__newArray
          p <- dummyPtr
          _ <- primIO $ prim__arrayAppendAnyPtr a 0 p
          pass

        , MkTest "prim__arrayIndexAnyPtr doesn't crash" $ do
          a <- primIO prim__newArray
          p <- dummyPtr
          a <- primIO $ prim__arrayAppendAnyPtr a 0 p
          -- TODO not convinced the following line will actually be executed
          let _ = prim__arrayIndexAnyPtr 0 a
          pass

        , MkTest "prim__arrayAppendInt doesn't crash" $ do
          a <- primIO prim__newArray
          let i = 3
          let j = 10
          a <- primIO $ prim__arrayAppendInt a 0 i
          a <- primIO $ prim__arrayAppendInt a 1 j
          pass

        , MkTest "prim__arrayIndexInt returns correct value" $ do
          a <- primIO prim__newArray
          let i = 6
          a <- primIO $ prim__arrayAppendInt a 0 i
          let j = prim__arrayIndexInt 0 a
          assertEq i j

        , MkTest "prim__newArray doesn't crash" $ do
          _ <- primIO prim__newArray
          pass

        , MkTest "prim__freeArray doesn't crash" $ do
          a <- primIO prim__newArray
          primIO $ prim__freeArray a
        ]
