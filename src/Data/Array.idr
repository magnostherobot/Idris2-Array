module Data.Array

import Data.Vect
import System.FFI
import public Control.Linear.LIO

import public Data.Array.Types
import Data.Array.Primitives

Cast (Fin n) Int where
  cast = cast . the Nat . cast

appendAnyPtr : {n : _} -> HasIO io =>
               (1 _ : Arr n AnyPtr) -> AnyPtr -> io $ Arr (S n) AnyPtr
appendAnyPtr (MkArr xs) x = do
  res' <- primIO $ prim__arrayAppendAnyPtr xs (cast n) x
  pure $ MkArr res'

indexAnyPtr : Fin n -> Arr n AnyPtr -> AnyPtr
indexAnyPtr i (MkArr xs) = prim__arrayIndexAnyPtr (cast i) xs

||| Interface used to add support for adding elements of a new type to arrays,
||| and indexing into arrays to fetch values from within.
public export
interface Appendable a where

  ||| Appends a value to an existing array.
  |||
  ||| @n number of items in the array prior to appending the new element.
  ||| @xs the array to append an element to.
  ||| @x the element to append to the array.
  append : {n : _} -> LinearIO io =>
           (1 xs : Arr n a) -> (x : a) -> L1 io $ Arr (S n) a

  ||| Indexes into an array to get a value.
  |||
  ||| @i the index of the value to fetch from the array.
  ||| @xs the array to index into.
  index : (i : Fin n) -> (xs : Arr n a) -> a

appendInt : {n : _} -> LinearIO io =>
            (1 xs : Arr n Int) -> (x : Int) -> L1 io $ Arr (S n) Int
appendInt (MkArr xs) x = do
  res' <- primIO $ prim__arrayAppendInt xs (cast n) x
  pure1 $ MkArr res'

indexInt : Fin n -> Arr n Int -> Int
indexInt i (MkArr xs) = prim__arrayIndexInt (cast i) xs

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

  eraseArr : (1 _ : Arr n e) -> Arr n AnyPtr
  eraseArr (MkArr xs) = MkArr xs

  unerase : AnyPtr -> e
  unerase = believe_me

  uneraseArr : (1 _ : Arr n AnyPtr) -> Arr n e
  uneraseArr (MkArr xs) = MkArr xs

appendErasableToAnyPtr : ErasableToAnyPtr e => LinearIO io =>
                         {n : _} -> (1 xs : Arr n e) -> e ->
                         L1 io $ Arr (S n) e
appendErasableToAnyPtr xs x = do let xs = eraseArr xs
                                 xs <- appendAnyPtr xs (erase x)
                                 let xs = uneraseArr xs
                                 pure1 xs

public export
ErasableToAnyPtr e => Appendable e where
  append xs x = appendErasableToAnyPtr xs x
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

||| Creates a new, empty array.
newArray : LinearIO io => L1 io $ Arr Z t
newArray = do arr <- primIO prim__newArray
              pure1 $ MkArr arr

public export
freeArray : HasIO io => (1 a : Arr n t) -> io ()
freeArray (MkArr x) = primIO $ prim__freeArray x

arrSizeLemma : {x : _} -> Vect x a -> (y : _) -> S x + y = x + S y
arrSizeLemma _ y = plusSuccRightSucc x y

foldlM : LinearIO io =>
         {j : _} ->
         (func : {n : _} -> (1 _ : Arr n a) -> a ->
                 L1 io $ Arr (S n) a) ->
         (1 acc : Arr j a) ->
         Vect k a ->
         L1 io $ Arr (k + j) a
foldlM func acc [] = pure1 acc
foldlM func acc (x :: xs) = do acc' <- func acc x
                               rewrite arrSizeLemma xs j
                               foldlM func acc' xs

public export
appendMany : LinearIO io => Appendable a => {n : _} ->
             (1 as : Arr n a) -> Vect m a -> L1 io $ Arr (m + n) a
appendMany as xs = foldlM append as xs

public export
toArray : LinearIO io => Appendable a => {n : _} ->
          Vect n a -> L1 io $ Arr n a
toArray xs =
  rewrite sym $ plusZeroRightNeutral n
  in appendMany !newArray xs

printArray : HasIO io => {n : _} -> Arr n AnyPtr -> io ()
printArray (MkArr xs) = primIO $ prim__printArray xs (cast n)

test : IO ()
test = run $ do
  a <- newArray {t = Int}
  a <- append a 10
  a <- appendMany a [2, 3, 3, 7]
  freeArray a

  b <- newArray {t = Int}
  freeArray b

  c <- toArray (the (Vect _ Int) [1, 6, 9])
  freeArray c

  pure ()
