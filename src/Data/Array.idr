module Data.Array

import Data.Vect
import System.FFI

arrayext : String -> String
arrayext x = "C:" ++ x ++ ",libidrarray"

||| Type used to represent C-style arrays.
|||
||| @n number of items in the array.
||| @t type of item contained in the array.
public export
data Arr : (n : Nat) -> (t : Type) -> Type where
  -- no constructors!

||| Type used by FFI wrapper functions to describe arrays.
||| Does not carry element type/count info as FFI wrappers cannot take implicit
||| arguments.
public export
Arr' : Type
Arr' = Ptr AnyPtr

||| Erase an array's type info.
||| Designed to be used internally; made public in case of emergencies.
public export
forgetArrType : Arr _ _ -> Arr'
forgetArrType = believe_me

||| Reintroduce an array's type info.
||| Designed to be used internally; made public in case of emergencies.
public export
recallArrType : Arr' -> Arr _ _
recallArrType = believe_me

Cast (Fin n) Int where
  cast = cast . the Nat . cast

%foreign (arrayext "array_append_anyptr")
prim__arrayAppendAnyPtr : Arr' -> Int -> AnyPtr -> PrimIO Arr'

appendAnyPtr : {n : _} -> HasIO io =>
               Arr n AnyPtr -> AnyPtr -> io $ Arr (S n) AnyPtr
appendAnyPtr xs x = do let xs' = forgetArrType xs
                       res' <- primIO $ prim__arrayAppendAnyPtr xs' (cast n) x
                       pure $ recallArrType res'

%foreign (arrayext "array_index_anyptr")
prim__arrayIndexAnyPtr : Int -> Arr' -> AnyPtr

indexAnyPtr : Fin n -> Arr n AnyPtr -> AnyPtr
indexAnyPtr i xs = prim__arrayIndexAnyPtr (cast i) (forgetArrType xs)

||| Interface used to add support for adding elements of a new type to arrays,
||| and indexing into arrays to fetch values from within.
public export
interface Appendable a where

  ||| Appends a value to an existing array.
  |||
  ||| @n number of items in the array prior to appending the new element.
  ||| @xs the array to append an element to.
  ||| @x the element to append to the array.
  append : {n : _} -> HasIO io =>
           (xs : Arr n a) -> (x : a) -> io $ Arr (S n) a

  ||| Indexes into an array to get a value.
  |||
  ||| @i the index of the value to fetch from the array.
  ||| @xs the array to index into.
  index : (i : Fin n) -> (xs : Arr n a) -> a

%foreign (arrayext "array_append_int")
prim__arrayAppendInt : Arr' -> Int -> Int -> PrimIO Arr'

appendInt : {n : _} -> HasIO io =>
            (xs : Arr n Int) -> (x : Int) -> io $ Arr (S n) Int
appendInt xs x = do let xs' = forgetArrType xs
                    res' <- primIO $ prim__arrayAppendInt xs' (cast n) x
                    pure $ recallArrType res'

%foreign (arrayext "array_index_int")
prim__arrayIndexInt : Int -> Arr' -> Int

indexInt : Fin n -> Arr n Int -> Int
indexInt i xs = prim__arrayIndexInt (cast i) (forgetArrType xs)

public export
Appendable Int where
  append = appendInt
  index = indexInt

||| Interface used to add array support for types that can be erased to AnyPtrs.
public export
interface ErasableToAnyPtr e where

  ||| Erases a type to an AnyPtr.
  |||
  ||| @e the type being erased.
  erase : e -> AnyPtr

  eraseArr : Arr n e -> Arr n AnyPtr
  eraseArr = believe_me

  unerase : AnyPtr -> e
  unerase = believe_me

  uneraseArr : Arr n AnyPtr -> Arr n e
  uneraseArr = believe_me

public export
ErasableToAnyPtr e => Appendable e where
  append xs x = do arr <- appendAnyPtr (eraseArr xs) (erase x)
                   pure $ uneraseArr arr
  index i xs = unerase $ indexAnyPtr i (eraseArr xs)

public export
ErasableToAnyPtr AnyPtr where
  erase = id

forget : Ptr t -> AnyPtr
forget = prim__forgetPtr

public export
ErasableToAnyPtr (Ptr t) where
  erase = forget

structToPtr : Struct _ _ -> AnyPtr
structToPtr = believe_me

public export
ErasableToAnyPtr (Struct n ms) where
  erase = structToPtr

%foreign (arrayext "new_array")
prim__newArray : PrimIO Arr'

||| Creates a new, empty array.
|||
||| @t the type of elements carried by the new array.
public export
newArray : HasIO io => (0 t : Type) -> io $ Arr Z t
newArray t = do arr <- primIO prim__newArray
                pure $ recallArrType arr

arrSizeLemma : {x : _} -> Vect x a -> (y : _) -> S x + y = x + S y
arrSizeLemma _ y = plusSuccRightSucc x y

foldlM : Monad m =>
         {j : _} ->
         (func : {n : _} -> Arr n a -> a -> m $ Arr (S n) a) ->
         (acc : Arr j a) ->
         Vect k a ->
         m $ Arr (k + j) a
foldlM func acc [] = pure acc
foldlM func acc (x :: xs) = do acc' <- func acc x
                               rewrite arrSizeLemma xs j
                               foldlM func acc' xs

public export
appendMany : HasIO io => Appendable a => {n : _} ->
             Arr n a -> Vect m a -> io $ Arr (m + n) a
appendMany as xs = foldlM append as xs

public export
toArray : HasIO io => Appendable a => {n : _} -> Vect n a -> io $ Arr n a
toArray xs =
  rewrite sym $ plusZeroRightNeutral n
  in appendMany !(newArray a) xs

%foreign (arrayext "print_array")
prim__printArray : Arr' -> Int -> PrimIO ()

printArray : HasIO io => {n : _} -> Arr n AnyPtr -> io ()
printArray xs = let xs' = forgetArrType xs
                in primIO $ prim__printArray xs' (cast n)
