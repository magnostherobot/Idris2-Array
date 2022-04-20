module ArrayTest

import System.FFI
import Test.Unit

import Array

public export
tests : PrimIO es => List (Test es)
tests = [ MkTest "newArray doesn't crash" $ do
          _ <- newArray ()
          pass
        
        , MkTest "append doesn't crash" $ do
          ptr <- malloc 0
          arr <- newArray AnyPtr
          arr' <- append arr ptr
          pass
        ]
