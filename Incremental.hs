{- |

Step 1: incremental computations.

The thing I find most confusing about iteratees, enumerators, and
enumeratees is I don't find the types trigger much intuition about
what the different entities do. So I'm going to start off by
implementing a very simplified version of iteratees, one in which you
cannot even do IO. The idea is to capture, as simply as possible, the
notions of incrementally consuming a stream of one kind of thing in
order to produce another kind of thing.

First, think about an incremental computation itself, one that
produces a value of type @b@ from a series of input values of type
@a@. But think about it in isolation from the series of inputs: that
is, somebody else is going to supply the series of inputs. There are
two fundamental states that such an computation can be in:

* it has finished computing the final value

* it needs to consume more values before it can produce the final
value

Well, actually, there is a third possible state: the computation may
have failed for some reason, say because the input series is in some
way badly formed. Think of an incremental computation for parsing a
line from a CSV file, for example.

Let's suppose also that the computation we're doing is pure. Here is
an attempt at a type that describes the possible states of an
incremental computation.
-}

module Incremental where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (isDigit)
import System.IO (Handle, hIsEOF, hClose, hGetChar, openFile, IOMode(..))

-- | An incremental computation that consumes zero or more values of
-- type @a@ to produce a value of type @b@.
--
-- The 'Partway' variant holds a function that continues the
-- computation to the next step given either the next input value, or
-- an indication that there is no more input. A well-formed 'Partway'
-- value holds a function that will not return another 'Partway' when
-- it's given 'Nothing'. At the end of the input, if we can produce a
-- result, we do, but if we can't we should signal an error.
data Incremental a b =
  Done b |
  -- ^ Success! We've produced an output value.
  Partway (Maybe a -> Incremental a b) |
  -- ^ We're partway through, and nothing bad has happened. Supply
  -- another input value to reach the next stage of the
  -- computation. Or, if there are no more input values, supply
  -- 'Nothing' and see how the computation ends.
  Error String
  -- ^ Something has gone horribly wrong.

-- | Here's an example, an incremental computation that counts the
-- number of unicode digits in a list of characters. Note that it
-- never fails, and produces a result if and only if there is no more
-- input. It also has a tail-recursive loop.
incCountDigits :: Incremental Char Int
incCountDigits = countStartingAt 0
  where countStartingAt n =
          Partway $ maybe (Done n) $ \c ->
          countStartingAt (if isDigit c then n+1 else n)

-- | An 'Incremental' computation holds the internal state of the
-- computation, and knows how to transform itself to a successor
-- state. We can write a function that runs an incremental computation
-- on a list of inputs.
--
-- Note that this function is tail-recursive, so it could compile to a
-- simple loop that runs in constant space. For an 'Incremental' that
-- can yield a value without seeing the end of the input, it can work
-- with non-ending lists.
runOnList :: [a] -> Incremental a b -> (Either String b, [a])
runOnList xs (Done b) = (Right b, xs)
runOnList [] (Partway f) = runOnList [] (f Nothing)
runOnList (x:xs) (Partway f) = runOnList xs (f $ Just x)
runOnList xs (Error s) = (Left s, xs)

-- | Now it's easy to define a function for counting digits. We'll be
-- careful about the error case, even though it should never happen.
countDigits :: String -> Int
countDigits s = case fst (runOnList s incCountDigits) of
  Left msg -> error $ "countDigits fails: " ++ msg
  Right r -> r

-- | Here is an incremental computation for breaking a prefix of @n@
-- elements from the head of a list.
incTake :: Int -> Incremental a [a]
incTake n = incTake' ([], n)
  where incTake' (rsofar, remaining) =
          Partway $ maybe (Done $ reverse rsofar) $ \x ->
          if remaining > 0 then incTake' (x:rsofar, remaining-1) else (Done $ reverse rsofar)

-- | Here's a version of 'take', named so as not to clash with the
-- prelude import. Looking at the source, notice that it's pretty much
-- identical to 'countDigits', so there is some code we'll factor out
-- in a second.
take' :: Int -> [a] -> [a]
take' n xs = case fst (runOnList xs $ incTake n) of
  Left msg -> error $ "take' fails: " ++ msg
  Right r -> r


-- | One of the problems with 'runOnList' is it assumes that the given
-- list is the complete source of input, which isn't entirely
-- flexible.
--
-- Imagine that we wanted to count the total digits in several
-- strings. One way would be to run 'countDigits' on each string,
-- maintaining a running total. That's a shame, because all the logic
-- for computing the running total is in 'incCountDigits'. If only
-- there were a way to feed an 'Incremental' input from a series of
-- sources.
--
-- In John Millikin's enumerator package, an enumerator as a function
-- from a step to an iteratee, where an iteratee is a newtype wrapper
-- around a (usually monadic) computation that returns a step. This is
-- almost a transformation from an iteratee to iteratee; the intended
-- transformation is achieved by feeding the iteratee a series of
-- input values from some stream, which is managed behind the scenes.
--
-- The 'Incremental' type here is like an iteratee without the monad
-- and newtype wrappers. The equivalent of an enumerator type would be
-- something like
--
--   Incremental a b -> Incremental a b
--
-- However, unlike Millikin's iteratees, our 'Done' state does not
-- carry unconsumed input, so perhaps the type should be more like
--
--   s -> Incremental a b -> (Incremental a b, s)
--
-- where @s@ is some type representing a source of input values.
--
-- The 'runOnList' function is almost such a function with @s =
-- [a]@. Here's a version that has the right type.
runOnList' :: [a] -> Incremental a b -> (Incremental a b, [a])
runOnList' [] (Partway f) = (f Nothing, [])
runOnList' (x:xs) (Partway f) = (f (Just x), xs)
runOnList' xs inc = (inc, xs)

-- | It departs from the usual iteratee practice, but it's curious
-- that there is no general type for an input source. The funny thing
-- about the usual defininition of enumerator is that although an
-- enumerator takes input from some source and feeds it to an
-- iteratee, there isn't really anything about the type that makes
-- that obvious. Obtaining input elements and freeing up resources
-- like open files is all left up to the internal plumbing of the
-- enumerator implementation.
--
-- What if instead we tried to represent sources as their own
-- datatype? Here is an attempt, where a source is a (generally
-- monadic) computation that may yield a new input element, and a
-- source for the rest of the input.
newtype Source m a =
  Source { runSource :: m (Maybe a, Source m a) }

-- | Here's the empty source.
empty :: Monad m => Source m a
empty = Source $ return (Nothing, empty)

-- | Here's a source for lists.
listSource :: Monad m => [a] -> Source m a
listSource [] = empty
listSource (x:xs) = Source $ return (Just x, listSource xs)

-- | Here's a source of characters from an IO handle. Like the
-- enumerators in the enumerator package, this source does not
-- automatically close the handle when it reaches the end of input.
handleCharSource :: Handle -> Source IO Char
handleCharSource h = Source $ do
  eof <- hIsEOF h
  (if eof
   then return (Nothing, empty)
   else hGetChar h >>= \c -> return (Just c, handleCharSource h))

-- | A source of characters from a named file. Closes the file when
-- the end of file is reached.
fileCharSource :: FilePath -> Source IO Char
fileCharSource fp =
  Source $ do
    h <- openFile fp ReadMode
    runSource $ safeCharSource h
    where safeCharSource h = Source $ do
            eof <- hIsEOF h
            (if eof
             then hClose h >> return (Nothing, empty)
             else hGetChar h >>= \c -> return (Just c, safeCharSource h))

-- | We can make a new source by concatenating two others.
cat :: Monad m => Source m a -> Source m a -> Source m a
cat s1 s2 = Source $ do
  (mi, s1') <- runSource s1
  maybe (runSource s2) (const $ return (mi, cat s1' s2)) mi

-- | And, we can write a generic 'runOnSource' function that is
-- analogous to 'runOnList'. Note that now running on a source is
-- always monadic, though it need not be effectful.
runOnSource :: Monad m => Source m a -> Incremental a b -> m (Incremental a b, Source m a)
runOnSource s (Partway f) = do
  (mi, s') <- runSource s
  runOnSource s' (f mi)
runOnSource s doneOrErr = return (doneOrErr, s)

-- | Consumes all input and results in nothing.
skipRest :: Incremental a ()
skipRest = Partway $ maybe (Done ()) (const skipRest)

finish :: Monad m => Source m a -> Incremental a b -> m (Either String b)
finish s inc = do
  (inc', s') <- runOnSource s inc
  runOnSource s' skipRest
  return $ case inc' of
    Done x -> Right x
    Partway _ -> Left "failed to produce a result"
    Error s -> Left s
