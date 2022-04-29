module Data.Array.Types

||| Type used by FFI wrapper functions to describe arrays.
||| Does not carry element type/count info as FFI wrappers cannot take implicit
||| arguments.
public export
Arr' : Type
Arr' = Ptr AnyPtr

||| Type used to represent C-style arrays.
|||
||| @n number of items in the array.
||| @t type of item contained in the array.
public export
data Arr : (n : Nat) -> (t : Type) -> Type where
  MkArr : Arr' -> Arr n t
