module HieSource.T17 where

data D = D { a :: (), b :: Int }

newtype T = T { unT :: Int }
  deriving (Eq, Show)
