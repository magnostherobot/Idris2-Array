module Data.Array.Primitives

import Data.Array.Types

arrayext : String -> String
arrayext x = "C:" ++ x ++ ",libidrarray"

public export
%foreign (arrayext "array_append_anyptr")
prim__arrayAppendAnyPtr : Arr' -> Int -> AnyPtr -> PrimIO Arr'

public export
%foreign (arrayext "array_index_anyptr")
prim__arrayIndexAnyPtr : Int -> Arr' -> AnyPtr

public export
%foreign (arrayext "array_append_int")
prim__arrayAppendInt : Arr' -> Int -> Int -> PrimIO Arr'

public export
%foreign (arrayext "array_index_int")
prim__arrayIndexInt : Int -> Arr' -> Int

public export
%foreign (arrayext "new_array")
prim__newArray : PrimIO Arr'

public export
%foreign (arrayext "print_array")
prim__printArray : Arr' -> Int -> PrimIO ()

public export
%foreign (arrayext "free_array")
prim__freeArray : Arr' -> PrimIO ()
