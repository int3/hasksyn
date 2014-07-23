{-# LANGUAGE CPP, ExistentialQuantification, KindSignatures, TypeFamilies,
    RecursiveDo #-}
module Test1 (
  -- * Types
  Type1,
  TF(..),
  -- * Functions
  func
  ) where

-- A normal comment
import Prelude hiding ( fst )
import qualified Control.Exception as E

-- Same names, different colors!
newtype MyInt = MyInt Int

-- Sum type, on one line
data Type0 a = T01 a Int | T02 [Double]

-- | On multiple lines, with some haddock
data Type1 a = TC1 a Int
             | TC2 [Double]
             -- ^ A special constructor
             | TC3
             deriving (Eq, Ord)

-- paren-less deriving
newtype Foo = Foo Int deriving Eq

#if defined(__GLASGOW_HASKELL__)
class TF a where
  data Bar :: * -> *
  data Baz :: *

  tf :: a -> Int -> Baz
#endif


func :: (Ord a, Eq a)
     => Int
     -> a
     -> IO (Type1 a)
func i a = do
  l <- getLine
  case l of
    'x' : _ -> return TC3
    _ -> do
      putStrLn "No 'x', but have a string pretending to let bindings"
      let foo = 100
          bar = 10 {- behave around ugly comments
          FIXME: With fixme highlighting
      return
      -}

      return$ TC1 a i

foo :: Int -> IO ()
foo = mdo
    return undefined
