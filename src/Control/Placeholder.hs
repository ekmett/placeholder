{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 980
#define WARNING_IN_XTODO WARNING in "x-todo"
#else
#define WARNING_IN_XTODO WARNING
#endif

module Control.Placeholder 
  ( todo, pattern TODO
  , unimplemented, pattern Unimplemented
  , todoIO
  , unimplementedIO
  , TodoException(TodoException, TodoExceptionWithLocation)
  , UnimplementedException(UnimplementedException, UnimplementedExceptionWithLocation)
  ) where

import Control.Exception
import Data.List (intercalate)
import Data.Typeable
import GHC.Base (raise#, raiseIO#, TYPE, RuntimeRep)
import GHC.Exception
import GHC.Stack
import GHC.Types (IO(IO))
import System.IO.Unsafe

-- | This is the 'Exception' thrown by 'todo', 'TODO' and 'todoIO'.
data TodoException = TodoExceptionWithLocation String
  deriving (Show, Typeable)

instance Exception TodoException where
  displayException (TodoExceptionWithLocation loc)
    = showString todoMessage $ showChar '\n' $ showString loc ""

pattern TodoException :: TodoException
pattern TodoException <- TodoExceptionWithLocation _ where
  TodoException = TodoExceptionWithLocation missingLocation

data UnimplementedException = UnimplementedExceptionWithLocation String
  deriving (Show, Typeable)

instance Exception UnimplementedException where
  displayException (UnimplementedExceptionWithLocation loc)
    = showString unimplementedMessage $ showChar '\n' $ showString loc ""

pattern UnimplementedException :: UnimplementedException
pattern UnimplementedException <- UnimplementedExceptionWithLocation _ where
  UnimplementedException = UnimplementedExceptionWithLocation missingLocation

-- | robust retrieval of the current callstack suitable for custom exception types
withCallStack :: Exception a => (String -> a) -> CallStack -> SomeException
withCallStack f stk = unsafeDupablePerformIO $ do
  ccsStack <- currentCallStack
  let
    implicitParamCallStack = prettyCallStackLines stk
    ccsCallStack = showCCSStack ccsStack
    stack = intercalate "\n" $ implicitParamCallStack ++ ccsCallStack
  return $ toException (f stack)

todo :: forall (r :: RuntimeRep) (a :: TYPE r). HasCallStack => a
todo = raise# (withCallStack TodoExceptionWithLocation ?callStack)
{-# WARNING_IN_XTODO todo "'todo' left in code" #-}

todoIO :: HasCallStack => IO a
todoIO = IO (raiseIO# (withCallStack TodoExceptionWithLocation ?callStack))
{-# WARNING_IN_XTODO todoIO "'todoIO' left in code" #-}

pattern TODO :: HasCallStack => () => a
pattern TODO <- (raise# (withCallStack TodoExceptionWithLocation ?callStack) -> _unused) where
  TODO = raise# (withCallStack TodoExceptionWithLocation ?callStack)
{-# WARNING_IN_XTODO TODO "'TODO' left in code" #-}
{-# COMPLETE TODO #-}

unimplemented :: forall (r :: RuntimeRep) (a :: TYPE r). HasCallStack => a
unimplemented = raise# (withCallStack TodoExceptionWithLocation ?callStack)

unimplementedIO :: HasCallStack => IO a
unimplementedIO = IO (raiseIO# (withCallStack UnimplementedExceptionWithLocation ?callStack))

pattern Unimplemented :: HasCallStack => () => a
pattern Unimplemented <- (raise# (withCallStack UnimplementedExceptionWithLocation ?callStack) -> _unused) where
  Unimplemented = raise# (withCallStack UnimplementedExceptionWithLocation ?callStack)
{-# COMPLETE Unimplemented #-}

missingLocation :: String
missingLocation = ""
{-# NOINLINE missingLocation #-}

todoMessage :: String
todoMessage = "Control.Placeholder.todo: not yet implemented"
{-# NOINLINE todoMessage #-}

unimplementedMessage :: String
unimplementedMessage = "Control.Placeholder.unimplemented: unimplemented"
{-# NOINLINE unimplementedMessage #-}

